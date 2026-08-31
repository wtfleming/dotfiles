#!/usr/bin/env bash
# Create and bootstrap a worktree at the merge base, so a probe can be run against
# the code as it was before the change.
#
# Bootstrap is the point. A worktree shares git history but not gitignored files, so
# `git worktree add` alone yields a tree with no dependencies, no compiled artifacts,
# no .env and no generated code. Failures from that state are indistinguishable from
# a real reproduction, which is how a broken baseline gets reported as a proven bug.
#
# By default the "after" side is your working checkout. Pass --head <ref> to name it
# explicitly and a second worktree is built for it too — needed when the change is
# already merged, or lives on a branch you do not want to check out. That also makes
# the two sides symmetric: both freshly bootstrapped, neither carrying whatever your
# working checkout has accumulated.
#
# Worktrees are created OUTSIDE the repository. Build tools infer their project root
# by walking up from the working directory, and a worktree nested under the checkout
# gives them two plausible roots to choose between.
#
# Ecosystems are detected from marker files at the repo root and can be forced or
# replaced outright. Detection is a convenience, not a contract: --install-cmd and
# --build-cmd override everything and are the right answer for anything unusual.
#
# Written for bash 3.2 (the macOS system bash), so no associative arrays, no mapfile.
#
# Usage:
#   baseline-worktree.sh create [--base <ref>] [--head <ref>] [--ecosystem <name>]...
#                               [--filter <pkg>] [--copy <relpath>]...
#                               [--install-cmd <cmd>] [--build-cmd <cmd>]
#                               [--no-build] [--force]
#   baseline-worktree.sh remove
#   baseline-worktree.sh path [baseline|head]
#   baseline-worktree.sh detect

set -euo pipefail

TOOL_PREFIX=()
OPT_FILTER=""
OPT_NO_BUILD=0
OPT_INSTALL_CMD=""
OPT_BUILD_CMD=""
OPT_COPIES=()
ECOSYSTEMS=()

die() {
  echo "error: $*" >&2
  exit 1
}

# Expands to nothing when empty, which plain "${arr[@]}" does not under set -u on bash 3.2.
prefix() {
  echo "${TOOL_PREFIX[@]+${TOOL_PREFIX[@]}}"
}

# Resolves the main checkout even when invoked from inside another worktree, where
# --show-toplevel would return the enclosing worktree and nest this one inside it.
main_root() {
  git rev-parse --git-common-dir >/dev/null 2>&1 || die "not inside a git repository"
  dirname "$(git rev-parse --path-format=absolute --git-common-dir)"
}

# worktree_path [baseline|head]
worktree_path() {
  local which=${1:-baseline} root
  root="${TMPDIR:-/tmp}/verify-baseline/$(basename "$(main_root)")"
  case "$which" in
    baseline) echo "$root" ;;
    head) echo "$root-head" ;;
    *) die "unknown worktree: $which (expected baseline or head)" ;;
  esac
}

# Registration lives in .git/worktrees, not in the directory, and $TMPDIR is purged on
# a timer -- so a worktree is routinely registered with nothing on disk, which blocks
# `git worktree add` until something prunes it. Pruning first is what makes the plain
# directory test below sufficient: it drops exactly the registrations that have no
# directory, so the two states agree afterwards.
worktree_present() {
  local main=$1 wt=$2
  git -C "$main" worktree prune
  [ -d "$wt" ]
}

# Either half can be the one that survived, so clear both and prune what git kept.
worktree_destroy() {
  local main=$1 wt=$2
  git -C "$main" worktree remove --force "$wt" 2>/dev/null || rm -rf "$wt"
  git -C "$main" worktree prune
}

# A repo may legitimately be several of these at once.
detect_ecosystems() {
  local root=$1 found=()
  [ -f "$root/package.json" ] && found+=(node)
  [ -f "$root/Cargo.toml" ] && found+=(rust)
  [ -f "$root/mix.exs" ] && found+=(elixir)
  { [ -f "$root/rebar.config" ] || [ -f "$root/erlang.mk" ]; } && found+=(erlang)
  { [ -f "$root/Eldev" ] || [ -f "$root/Cask" ] || compgen -G "$root/*.el" >/dev/null; } && found+=(elisp)
  echo "${found[*]:-}"
}

has_tool_config() {
  local root=$1
  [ -f "$root/.tool-versions" ] || [ -f "$root/mise.toml" ] || [ -f "$root/.mise.toml" ]
}

# Version managers pin the toolchain per directory. The pin lives in a tracked file,
# so a worktree inherits it — but only if the manager is actually invoked. A
# non-interactive shell usually has not run the manager's shell hook, so `mix` and
# friends are simply absent from PATH, and the resulting "command not found" in the
# baseline reads exactly like a broken tree.
set_tool_prefix() {
  local root=$1
  TOOL_PREFIX=()
  has_tool_config "$root" || return 0
  if command -v mise >/dev/null 2>&1; then
    TOOL_PREFIX=(mise exec --)
  elif command -v asdf >/dev/null 2>&1 && [ -f "$root/.tool-versions" ]; then
    TOOL_PREFIX=(asdf exec)
  fi
}

eco_binary() {
  case "$1" in
    node) echo node ;;
    rust) echo cargo ;;
    elixir) echo mix ;;
    erlang) echo rebar3 ;;
    elisp) echo emacs ;;
  esac
}

# Fail before creating anything, with a message that names the cause. A missing
# toolchain discovered halfway through a bootstrap looks like a build failure.
require_toolchain() {
  local eco=$1 root=$2 bin
  bin="$(eco_binary "$eco")"
  command -v "$bin" >/dev/null 2>&1 && return 0
  [ ${#TOOL_PREFIX[@]} -gt 0 ] && return 0
  if has_tool_config "$root"; then
    local pins="" f
    for f in .tool-versions mise.toml .mise.toml; do
      [ -f "$root/$f" ] && pins="$pins $f"
    done
    die "$bin is not on PATH and this project pins its toolchain (${pins# }), but neither mise nor asdf is installed. Install the version manager, or pass --install-cmd/--build-cmd with the invocation you actually use."
  fi
  die "$bin is not on PATH. Install it, or pass --install-cmd/--build-cmd."
}

detect_node_pm() {
  local root=$1
  if [ -f "$root/pnpm-lock.yaml" ]; then echo pnpm
  elif [ -f "$root/yarn.lock" ]; then echo yarn
  elif [ -f "$root/bun.lockb" ] || [ -f "$root/bun.lock" ]; then echo bun
  else echo npm
  fi
}

run_in() {
  local wt=$1; shift
  echo "    \$ $(prefix) $*"
  (cd "$wt" && ${TOOL_PREFIX[@]+"${TOOL_PREFIX[@]}"} "$@")
}

install_for() {
  local eco=$1 main=$2 wt=$3 pm
  case "$eco" in
    node)
      pm="$(detect_node_pm "$main")"
      run_in "$wt" "$pm" install
      ;;
    rust)
      # Populates the shared ~/.cargo registry cache; the per-tree target/ is built below.
      run_in "$wt" cargo fetch
      ;;
    elixir)
      run_in "$wt" mix deps.get
      ;;
    erlang) ;;  # rebar3 fetches as part of compile
    elisp)
      if [ -f "$main/Eldev" ]; then run_in "$wt" eldev prepare
      elif [ -f "$main/Cask" ]; then run_in "$wt" cask install
      fi
      ;;
  esac
}

build_for() {
  local eco=$1 main=$2 wt=$3 pm
  case "$eco" in
    node)
      pm="$(detect_node_pm "$main")"
      if [ -f "$main/turbo.json" ]; then
        # Through the task runner, never a package's own build script: the latter
        # skips the dependency graph, so upstream artifacts and codegen never run
        # and the build fails with resolution errors that look like code defects.
        if [ -n "$OPT_FILTER" ]; then
          run_in "$wt" "$pm" exec -- turbo run build --filter="$OPT_FILTER"
        else
          run_in "$wt" "$pm" exec -- turbo run build
        fi
      elif grep -q '"build"' "$main/package.json" 2>/dev/null; then
        [ -n "$OPT_FILTER" ] && echo "    (--filter $OPT_FILTER ignored: no turbo.json, so the build is not scoped)"
        run_in "$wt" "$pm" run build
      else
        echo "    (no node build step found)"
      fi
      ;;
    rust)
      if [ -n "$OPT_FILTER" ]; then
        run_in "$wt" cargo build --offline -p "$OPT_FILTER"
      else
        run_in "$wt" cargo build --offline
      fi
      ;;
    elixir)
      # MIX_ENV splits the build tree. A probe that is a test runs under `test`, and
      # a dev-profile compile does nothing for it; mix test compiles what it needs,
      # so this warms dev and leaves the test profile to the probe itself.
      run_in "$wt" mix compile
      ;;
    erlang) run_in "$wt" rebar3 compile ;;
    elisp)
      # Byte-compiling is deliberately skipped. A stale .elc shadows the .el beside
      # it, so compiling only one of the two trees is a way to compare artifacts
      # rather than sources. Run the probe from source on both sides.
      echo "    (skipping byte-compilation on purpose — see references/environments.md)"
      ;;
  esac
}

# Copy rather than symlink generated artifacts. A symlinked input makes a build tool
# hash a file outside the worktree, which poisons its cache for every tree sharing it.
copy_if_ignored() {
  local main=$1 relative=$2 dest=$3
  local rc
  (cd "$main" && git check-ignore -q "$relative") && rc=0 || rc=$?
  case $rc in
    0)
      mkdir -p "$(dirname "$dest")"
      cp -R "$main/$relative" "$dest"
      echo "    copied $relative"
      ;;
    1)
      echo "    skipped $relative (tracked in git — the worktree already has it)"
      ;;
    *)
      echo "warn: could not determine gitignore state of $relative; skipped" >&2
      ;;
  esac
}

# The generic bootstrap-gap detector. Anything gitignored that the main checkout has
# and a worktree does not is a candidate for the "fails for environmental reasons"
# verdict, whatever the language.
report_missing_ignored() {
  local main=$1 wt=$2 missing=() path
  while IFS= read -r path; do
    [ -n "$path" ] || continue
    [ -e "$wt/$path" ] || missing+=("$path")
  done < <(git -C "$main" status --porcelain --ignored=traditional 2>/dev/null | sed -n 's/^!! //p')

  [ ${#missing[@]} -gt 0 ] || return 0
  echo
  echo "    gitignored in the main checkout, absent here:"
  printf '      %s\n' "${missing[@]:0:20}"
  [ ${#missing[@]} -gt 20 ] && echo "      … and $(( ${#missing[@]} - 20 )) more"
  echo "    Most are build output this tree does not need. Bring one over with"
  echo "    --copy <path> if a failure names it."
  return 0
}

# A change that bumps the pinned toolchain makes the two trees differ by more than
# their source, which is worth knowing before reading anything into the result.
report_toolchain_drift() {
  local main=$1 from=$2 to=$3 changed
  changed="$(git -C "$main" diff --name-only "$from" "$to" -- \
    .tool-versions mise.toml .mise.toml 2>/dev/null || true)"
  [ -n "$changed" ] || return 0
  echo
  echo "note: this change edits the pinned toolchain ($(echo "$changed" | tr '\n' ' '))."
  echo "  The two trees will run on different tool versions. That may be the point,"
  echo "  but it means the differential is not isolating source alone."
  return 0
}

# When --head is already merged into --base, the merge base collapses onto head and
# comparing the two says nothing. The useful baseline is where head's line forked,
# which is recoverable from the merge commit that brought it in. Suggested rather
# than applied: silently comparing something other than what was asked is how a
# verification stops meaning what its report claims.
suggest_fork_point() {
  local main=$1 after=$2 base=$3 merge_commit p1 p2
  merge_commit="$(git -C "$main" rev-list --ancestry-path --merges "$after".."$base" 2>/dev/null | tail -1)"
  [ -n "$merge_commit" ] || return 0
  p1="$(git -C "$main" rev-parse --verify "$merge_commit^1" 2>/dev/null)" || return 0
  p2="$(git -C "$main" rev-parse --verify "$merge_commit^2" 2>/dev/null)" || return 0
  git -C "$main" merge-base "$p1" "$p2" 2>/dev/null || return 0
}

# add_tree <main> <worktree-path> <commit> <label>
add_tree() {
  local main=$1 wt=$2 commit=$3 label=$4 eco relative env_file

  echo "==> $label at $(git -C "$main" rev-parse --short "$commit")"
  mkdir -p "$(dirname "$wt")"
  (cd "$main" && git worktree add --detach "$wt" "$commit")

  # Symlinked, not copied: .env holds the machine's real credentials, and a copy in a
  # temp worktree outlives the run. The cache concern above is about build inputs the
  # tool hashes; .env is read at run time.
  echo "    linking .env files"
  while IFS= read -r env_file; do
    relative="${env_file#"$main"/}"
    # A tracked .env is part of the code under test, and this commit's own copy is
    # already checked out -- overwriting it would run the baseline on HEAD's config.
    if git -C "$wt" ls-files --error-unmatch "$relative" >/dev/null 2>&1; then
      echo "    skipped $relative (tracked at this commit — the worktree has its own)"
      continue
    fi
    mkdir -p "$wt/$(dirname "$relative")"
    ln -sf "$env_file" "$wt/$relative"
  done < <(find "$main" -name ".env" -not -path "*/node_modules/*" -not -path "*/.git/*" \
    -not -path "*/deps/*" -not -path "*/_build/*" -not -path "*/target/*")

  if [ ${#OPT_COPIES[@]} -gt 0 ]; then
    echo "    copying generated artifacts"
    for relative in "${OPT_COPIES[@]}"; do
      if [ -e "$main/$relative" ]; then
        copy_if_ignored "$main" "$relative" "$wt/$relative"
      else
        echo "warn: $relative does not exist in the main checkout; skipped" >&2
      fi
    done
  fi

  echo "    install"
  if [ -n "$OPT_INSTALL_CMD" ]; then
    echo "    \$ $OPT_INSTALL_CMD"
    (cd "$wt" && eval "$OPT_INSTALL_CMD")
  elif [ ${#ECOSYSTEMS[@]} -eq 0 ]; then
    echo "    (nothing detected — pass --install-cmd if this project needs one)"
  else
    for eco in "${ECOSYSTEMS[@]}"; do install_for "$eco" "$main" "$wt"; done
  fi

  echo "    build"
  if [ "$OPT_NO_BUILD" -eq 1 ]; then
    echo "    (skipped: --no-build)"
  elif [ -n "$OPT_BUILD_CMD" ]; then
    echo "    \$ $OPT_BUILD_CMD"
    (cd "$wt" && eval "$OPT_BUILD_CMD")
  elif [ ${#ECOSYSTEMS[@]} -eq 0 ]; then
    echo "    (nothing detected — pass --build-cmd if this project needs one)"
  else
    for eco in "${ECOSYSTEMS[@]}"; do build_for "$eco" "$main" "$wt"; done
  fi

  report_missing_ignored "$main" "$wt"
}

cmd_create() {
  local base="main" head_ref="" force=0 eco
  while [ $# -gt 0 ]; do
    case "$1" in
      --base) base="${2:?--base needs a ref}"; shift 2 ;;
      --head) head_ref="${2:?--head needs a ref}"; shift 2 ;;
      --ecosystem)
        case "${2:?--ecosystem needs a name}" in
          node|rust|elixir|erlang|elisp) ECOSYSTEMS+=("$2") ;;
          *) die "unknown ecosystem: $2 (expected node|rust|elixir|erlang|elisp)" ;;
        esac
        shift 2 ;;
      --filter) OPT_FILTER="${2:?--filter needs a package}"; shift 2 ;;
      --copy) OPT_COPIES+=("${2:?--copy needs a path}"); shift 2 ;;
      --install-cmd) OPT_INSTALL_CMD="${2:?--install-cmd needs a command}"; shift 2 ;;
      --build-cmd) OPT_BUILD_CMD="${2:?--build-cmd needs a command}"; shift 2 ;;
      --no-build) OPT_NO_BUILD=1; shift ;;
      --force) force=1; shift ;;
      *) die "unknown argument: $1" ;;
    esac
  done

  local main wt wt_head merge_base after after_label existing=()
  main="$(main_root)"
  wt="$(worktree_path baseline)"
  wt_head="$(worktree_path head)"

  if [ -n "$head_ref" ]; then
    after="$(git -C "$main" rev-parse --verify "$head_ref^{commit}" 2>/dev/null)" \
      || die "--head $head_ref does not resolve to a commit"
    after_label="$head_ref"
  else
    after="$(git -C "$main" rev-parse HEAD)"
    after_label="your working checkout"
  fi

  # Without --head the working checkout is the head side, so uncommitted edits run on
  # one side of the comparison and not the other -- they would pass as the change.
  if [ -z "$head_ref" ] && ! git -C "$main" diff --quiet HEAD; then
    die "the working checkout is dirty, and without --head it is the head side of the comparison. Uncommitted edits would be credited to the change. Commit or stash them, or name the change with --head <ref>."
  fi

  merge_base="$(git -C "$main" merge-base "$after" "$base")" \
    || die "no merge base between $after_label and $base"

  if [ "$merge_base" = "$after" ]; then
    if [ -n "$head_ref" ]; then
      local fork
      fork="$(suggest_fork_point "$main" "$after" "$base")"
      if [ -n "$fork" ]; then
        die "$head_ref is already merged into $base, so $base contains it and the merge base collapses onto it. Its branch forked at $(git -C "$main" rev-parse --short "$fork"). Rerun with: --head $head_ref --base $(git -C "$main" rev-parse --short "$fork")"
      fi
      die "$head_ref is the merge base with $base — nothing lies between them. Name a --base that predates the change."
    fi
    die "HEAD is the merge base with $base — there is no committed change to verify. Commit your work, pass a --base you actually branched from, or name the change with --head <ref> if it is already merged."
  fi

  if [ ${#ECOSYSTEMS[@]} -eq 0 ]; then
    read -r -a ECOSYSTEMS <<< "$(detect_ecosystems "$main")"
  fi

  set_tool_prefix "$main"

  # Preflight before any mutation, unless the caller replaced both steps anyway.
  if [ -z "$OPT_INSTALL_CMD" ] && [ -z "$OPT_BUILD_CMD" ]; then
    for eco in ${ECOSYSTEMS[@]+"${ECOSYSTEMS[@]}"}; do
      require_toolchain "$eco" "$main"
    done
  fi

  worktree_present "$main" "$wt" && existing+=("$wt")
  if [ -n "$head_ref" ] && worktree_present "$main" "$wt_head"; then existing+=("$wt_head"); fi
  if [ ${#existing[@]} -gt 0 ]; then
    if [ "$force" -eq 1 ]; then
      local e
      for e in "${existing[@]}"; do worktree_destroy "$main" "$e"; done
    else
      die "${existing[*]} already exists. Reuse it, or pass --force to recreate."
    fi
  fi

  echo "    ecosystems: ${ECOSYSTEMS[*]:-none detected}"
  [ ${#TOOL_PREFIX[@]} -gt 0 ] && echo "    toolchain:  $(prefix) (pinned by this project)"

  add_tree "$main" "$wt" "$merge_base" "baseline (merge-base of $after_label and $base)"
  if [ -n "$head_ref" ]; then
    add_tree "$main" "$wt_head" "$after" "head ($head_ref)"
  fi

  report_toolchain_drift "$main" "$merge_base" "$after"

  echo
  echo "baseline: $wt"
  if [ -n "$head_ref" ]; then
    echo "head:     $wt_head"
    echo "Both sides are worktrees, so the probe runs identically in each — and neither"
    echo "carries what your working checkout has accumulated."
  else
    echo "head:     your working checkout ($main)"
  fi
  if [ ${#TOOL_PREFIX[@]} -gt 0 ]; then
    echo "Run the probe on BOTH sides through the same prefix: $(prefix) <your command>"
  fi
  echo "Before trusting a failure, run a control that must pass on both sides."
  echo "Tear down with: $0 remove"
}

cmd_remove() {
  local main wt removed=0 which
  main="$(main_root)"
  for which in baseline head; do
    wt="$(worktree_path "$which")"
    if worktree_present "$main" "$wt"; then
      worktree_destroy "$main" "$wt"
      echo "removed $wt"
      removed=1
    fi
  done
  [ "$removed" -eq 1 ] || echo "nothing to remove"
}

case "${1:-}" in
  create) shift; cmd_create "$@" ;;
  remove) cmd_remove ;;
  path) shift; worktree_path "${1:-baseline}" ;;
  detect)
    main_dir="$(main_root)"
    echo "ecosystems: $(detect_ecosystems "$main_dir")"
    set_tool_prefix "$main_dir"
    if [ ${#TOOL_PREFIX[@]} -gt 0 ]; then
      echo "toolchain:  $(prefix)"
    elif has_tool_config "$main_dir"; then
      echo "toolchain:  pinned by this project, but neither mise nor asdf is installed"
    fi
    ;;
  *) die "usage: $0 create [--base <ref>] [--head <ref>] [--ecosystem <name>]... [--filter <pkg>] [--copy <relpath>]... [--install-cmd <cmd>] [--build-cmd <cmd>] [--no-build] [--force] | remove | path [baseline|head] | detect" ;;
esac
