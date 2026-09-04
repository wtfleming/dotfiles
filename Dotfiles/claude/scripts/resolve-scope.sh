#!/usr/bin/env bash
# Resolve "what code is under review" once, in code, and write it down.
#
# Several tools here -- /wtf-code-review and its lenses, /wtf-code-review-no-lenses,
# wtf-change-reviewer, wtf-design-reviewer, wtf-code-verify -- work the scope out from
# prose instructions. Under --deep that is eight lens agents plus one refuter per finding,
# each running its own git commands, and "the same scope" holds only for as long as every
# one of them derives it identically. This produces the diff once, writes it to a file,
# and hands every agent the path.
#
# The second thing it settles is whether the working tree actually holds the code under
# review. Reviewing HEAD~3, or a branch that is not checked out, is ordinary -- and in
# both the files on disk are not the files being reviewed. An agent reading the working
# tree there judges the wrong code, and wtf-refuter answers `refuted` when it cannot
# decide, so the mismatch does not add noise: it silently deletes true findings. Hence
# `correspondence`, which callers disclose and adapt to rather than abort on.
#
# One invariant the consumers rely on: **if manifest.json exists, the scope is non-empty.**
# Every path that would describe an empty scope either falls through to the next step or
# exits without writing anything. An empty diff on disk is indistinguishable from a
# collapsed range, and that confusion is a confident report about nothing.
#
# What it deliberately does not do: resolve a prose *subject* ("login and
# authentication"). There is no diff to produce. A scope that is not a PR, a range, a ref
# or a path exits 2 -- a distinct code, so a caller can branch to its subject path rather
# than parse an error string.
#
# Written for bash 3.2 (the macOS system bash), so no associative arrays, no mapfile.
#
# Usage:
#   resolve-scope.sh resolve [--scope <ref|range|path|PR#>] [--base <ref>] [--no-fetch]
#   resolve-scope.sh base
#   resolve-scope.sh path [--scope <arg>]

set -Eeuo pipefail

WARNINGS=()
FELL_THROUGH=()
FETCH_WARNING=""
FILE_LIST_SOURCE=git-apply-numstat

die() {
  echo "error: $*" >&2
  exit 1
}

warn() {
  WARNINGS+=("$*")
}

# Resolves the main checkout even when invoked from inside a linked worktree, where
# --show-toplevel returns the enclosing worktree. Same reason as baseline-worktree.sh.
repo_root() {
  git rev-parse --git-common-dir >/dev/null 2>&1 || die "not inside a git repository"
  dirname "$(git rev-parse --path-format=absolute --git-common-dir)"
}

hash12() {
  printf '%s' "$1" | shasum -a 256 | cut -c1-12
}

# Take the first candidate that resolves to a commit. Validating the result rather than
# trusting the source is the whole point: origin/HEAD can be a dangling symbolic ref after
# the upstream default branch is renamed, a local name can be missing in a fresh clone, and
# the prefix can be stripped off a ref that needed it. All three produce a base that looks
# resolved and is not.
#
# Written as `if ... then ... fi` rather than the `&& base=$c && break` form this is lifted
# from: that statement's value is 1 when a candidate misses, which under `set -e` kills the
# script on the first non-resolving candidate -- in exactly the repos the loop exists for.
resolve_default_branch() {
  local c
  for c in "$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true)" \
           origin/main origin/master origin/trunk main master trunk; do
    [ -n "$c" ] || continue
    if git rev-parse --verify --quiet "$c^{commit}" >/dev/null 2>&1; then
      echo "$c"
      return 0
    fi
  done
  return 1
}

# Refresh the base before it is used as a merge base. A stale remote-tracking ref does not
# fail, it just moves the merge base back, and the review scope grows to include commits
# nobody asked to review. Non-fatal on purpose -- offline review is legitimate -- so the
# failure is recorded and disclosed instead of raised. Narrow rather than --all: this is an
# unadvertised network call on every review and it should cost as little as it can.
fetch_base() {
  local base=$1 remote branch
  case "$base" in
    # A local name is only ever chosen when no remote-tracking ref resolved, so there is no
    # remote for it to be behind. Warning there would put a staleness note on every review
    # in a local-only repo, which spends the credibility of the real one.
    */*) remote="${base%%/*}"; branch="${base#*/}" ;;
    *)   return 0 ;;
  esac
  if ! git fetch --quiet "$remote" "$branch" 2>/dev/null; then
    FETCH_WARNING="git fetch $remote $branch failed; the merge base may be behind $remote"
  fi
}

# Keyed on the repo *and* the scope. The repo half carries a hash of the absolute path
# because two checkouts can share a basename -- for baseline-worktree.sh that collision is
# loud, since `git worktree add` refuses an existing directory, but here it would silently
# hand one repo's diff to the other's agents. The scope half keeps two scopes in one repo
# from overwriting each other, and is a pure function of the string, so `path` can answer
# without resolving anything.
scope_out_dir() {
  local scope=$1 root=$2 tmp slug
  tmp="${TMPDIR:-/tmp}"
  slug="$(printf '%s' "${scope:-auto}" | tr -c 'A-Za-z0-9._-' '-' | cut -c1-40)"
  echo "${tmp%/}/wtf-scope/$(basename "$root")-$(hash12 "$root")/${slug}-$(hash12 "${scope:-auto}")"
}

# Order matters. A PR number is checked before a ref because bare digits almost never name
# one; a ref beats a path, which is git's own convention; a path is last.
scope_shape_of() {
  local scope=$1 full
  [ -n "$scope" ] || { echo worktree; return 0; }
  case "$scope" in
    '#'[0-9]*) echo pr; return 0 ;;
    ''|*[!0-9]*) ;;
    *) echo pr; return 0 ;;
  esac
  case "$scope" in
    *..*) echo range; return 0 ;;
  esac
  full="$(git rev-parse --symbolic-full-name "$scope" 2>/dev/null || true)"
  case "$full" in
    # A branch means "against its merge base"; any other commit-ish means that commit
    # alone. Conflating them reviews one commit of a branch that has many.
    refs/heads/*|refs/remotes/*) echo branch; return 0 ;;
  esac
  if git rev-parse --verify --quiet "$scope^{commit}" >/dev/null 2>&1; then
    [ -e "$scope" ] && warn "'$scope' is both a ref and a path; read as a ref (pass ./$scope for the path)"
    echo commit
    return 0
  fi
  [ -e "$scope" ] && { echo path; return 0; }
  cat >&2 <<EOF
error: not a PR number, a range, a ref or an existing path: $scope
  If this is a subject -- prose naming an area of behaviour -- there is no diff to
  resolve and this is the wrong tool. The caller classifies subjects itself.
EOF
  exit 2
}

# Untracked files are uncommitted work that `git diff` never lists, and a new source file
# beside a Markdown edit is exactly the change that would otherwise pass as prose. They go
# into the same diff as everything else so no agent has to rediscover them.
#
# --no-index against /dev/null produces a real `new file mode` hunk with the right path,
# and reduces a binary to a single "Binary files ... differ" line, so neither needs
# synthesising here. It exits 1 whenever the files differ, which is always -- unguarded,
# that kills the script on the first untracked file.
append_untracked() {
  local out=$1 filter=$2 f rc
  while IFS= read -r -d '' f; do
    rc=0
    git diff --no-index -- /dev/null "$f" >> "$out" || rc=$?
    # Exit 1 is "they differ", which is every file here. Anything above it is a real
    # failure on one file, and one bad file must not cost the whole scope.
    [ "$rc" -le 1 ] || warn "could not diff untracked file: $f (git exit $rc)"
  done < <(git ls-files --others --exclude-standard -z ${filter:+-- "$filter"})
}

# `git diff <ref>^!` is shorthand for `<ref>^ <ref>` and has no parent to name on a root
# commit, where the whole tree is the change.
diff_one_commit() {
  local ref=$1
  if git rev-parse --verify --quiet "$ref^" >/dev/null 2>&1; then
    git diff "$ref^!"
  else
    git diff-tree -p --root "$ref"
  fi
}

# The file list is projected from the finished artifact, never from a second git command.
# Two commands can disagree about what the scope holds; one artifact cannot disagree with
# itself, which is the whole reason the diff is materialised. `git apply --numstat` reads
# it, because it handles the two shapes a header parser silently drops -- a
# 100%-similarity rename, which has no `+++` line at all, and a mode-only change.
# The file list is projected from the finished artifact, never from a second git command.
# Two commands can disagree about what the scope holds; one artifact cannot disagree with
# itself, which is the whole reason the diff is materialised. `git apply --numstat` reads
# it, because it handles the two shapes a header parser silently drops -- a
# 100%-similarity rename, which has no `+++` line at all, and a mode-only change.
#
# -z and jq, not awk: a path may contain a newline, and the BSD awk on macOS cannot use NUL
# as a record separator at all -- it silently processes only the first record, which would
# report a one-file scope for a diff of twenty. jq is already required here for the
# manifest, so this costs no new dependency.
files_from_diff() {
  local diff=$1 dest=$2
  if git apply --numstat -z "$diff" > "$dest.raw" 2>/dev/null; then
    jq -Rs 'split("\u0000") | map(select(length > 0) | sub("^[^\t]*\t[^\t]*\t"; ""))' \
      "$dest.raw" > "$dest"
  else
    FILE_LIST_SOURCE=fallback-headers
    warn "git apply could not parse the diff; the file list was recovered from its headers"
    sed -n 's|^+++ b/||p' "$diff" \
      | jq -Rs 'split("\n") | map(select(length > 0))' > "$dest"
  fi
  rm -f "$dest.raw"
}

# NUL-delimited into jq rather than split on newlines: a path or a warning containing a
# newline would otherwise arrive as two array elements.
json_array_from_lines() {
  printf '%s\0' "$@" | jq -Rs 'split("\u0000") | map(select(length > 0))'
}

cmd_resolve() {
  local scope="" base_override="" no_fetch=false
  while [ $# -gt 0 ]; do
    case "$1" in
      --scope)    scope="${2?--scope needs a value}"; shift 2 ;;
      --base)     base_override="${2:?--base needs a ref}"; shift 2 ;;
      --no-fetch) no_fetch=true; shift ;;
      *) die "unknown argument: $1" ;;
    esac
  done

  local root out shape base="" base_sha="" scope_head="" head_label="" resolved_by=""
  local resolution_step=explicit base_resolved=false
  root="$(repo_root)"
  cd "$root"
  git rev-parse --verify --quiet HEAD >/dev/null 2>&1 \
    || die "this repository has no commits; there is nothing to resolve a scope against"

  out="$(scope_out_dir "$scope" "$root")"
  shape="$(scope_shape_of "$scope")"

  # A PR, a range, a single commit, a path and the working tree all carry their own
  # endpoints; only a branch scope and the auto fall-through need a base.
  if [ -n "$base_override" ]; then
    git rev-parse --verify --quiet "$base_override^{commit}" >/dev/null 2>&1 \
      || die "--base $base_override does not resolve to a commit"
    base="$base_override"
    base_resolved=true
  elif base="$(resolve_default_branch)"; then
    base_resolved=true
  else
    base=""
  fi
  if [ "$base_resolved" = true ]; then
    [ "$no_fetch" = true ] || fetch_base "$base"
    base_sha="$(git rev-parse "$base")"
  fi

  # Build into a temporary directory and move it into place last, so a consumer that finds
  # a manifest finds a complete scope.diff beside it.
  local tmp="$out.tmp.$$" diff
  rm -rf "$tmp"
  mkdir -p "$tmp"
  trap 'rm -rf "$tmp"' EXIT
  diff="$tmp/scope.diff"
  : > "$diff"

  case "$shape" in
    pr)
      local n="${scope#\#}"
      command -v gh >/dev/null 2>&1 || die "scope '$scope' is a PR but gh is not installed"
      # gh pr diff is the single authority for a PR, and a failure is a stop rather than a
      # fallback. A locally computed base...head is a *different change*: the local base ref
      # may be stale, the PR may target a non-default base, and the PR may have been
      # rebased. Silently reviewing a near-miss is worse than not reviewing.
      gh pr diff "$n" > "$diff" \
        || die "gh pr diff $n failed. Not falling back to a local diff, which would review something other than the PR."
      scope_head="$(gh pr view "$n" --json headRefOid -q .headRefOid 2>/dev/null || true)"
      head_label="PR #$n head"
      resolved_by="gh pr diff $n"
      ;;
    range)
      git diff "$scope" > "$diff"
      scope_head="$(git rev-parse --verify --quiet "${scope##*.}^{commit}" 2>/dev/null || true)"
      head_label="$scope"
      resolved_by="git diff $scope"
      ;;
    commit)
      diff_one_commit "$scope" > "$diff"
      scope_head="$(git rev-parse "$scope^{commit}")"
      head_label="$scope"
      resolved_by="git diff $scope^!"
      ;;
    branch)
      [ "$base_resolved" = true ] \
        || die "cannot resolve a default branch to diff '$scope' against; pass --base"
      local mb
      mb="$(git merge-base "$base" "$scope")" \
        || die "no merge base between $scope and $base"
      base_sha="$mb"
      git diff "$mb...$scope" > "$diff"
      scope_head="$(git rev-parse "$scope^{commit}")"
      head_label="$scope"
      resolved_by="git diff \$(git merge-base $base $scope)...$scope"
      ;;
    path)
      git diff HEAD -- "$scope" > "$diff"
      append_untracked "$diff" "$scope"
      scope_head="$(git rev-parse HEAD)"
      head_label="working tree"
      resolved_by="git diff HEAD -- $scope, plus untracked files under it"
      ;;
    worktree)
      # The three-step order from scope-resolution.md: uncommitted work, else the branch
      # against its merge base, else the commit at HEAD. An empty result at any step means
      # *fall through*, never "no changes" -- so each step that produces nothing records
      # why, and the reasons are carried into the manifest even when a later step succeeds.
      git diff HEAD > "$diff"
      append_untracked "$diff" ""
      scope_head="$(git rev-parse HEAD)"
      head_label="working tree"
      resolved_by="git diff HEAD, plus untracked files"
      resolution_step=auto-1-worktree

      if [ ! -s "$diff" ]; then
        FELL_THROUGH+=("auto-1-worktree: no uncommitted or untracked changes")
        if [ "$base_resolved" = true ]; then
          local mb2
          if mb2="$(git merge-base "$base" HEAD 2>/dev/null)"; then
            base_sha="$mb2"
            git diff "$mb2...HEAD" > "$diff"
            shape=branch
            head_label=HEAD
            resolved_by="git diff \$(git merge-base $base HEAD)...HEAD"
            resolution_step=auto-2-branch
            [ -s "$diff" ] || FELL_THROUGH+=("auto-2-branch: empty diff against the merge base with $base (standing on the default branch, or the branch has no commits of its own)")
          else
            FELL_THROUGH+=("auto-2-branch: no merge base between HEAD and $base")
          fi
        else
          FELL_THROUGH+=("auto-2-branch: no default branch resolved")
        fi
      fi

      if [ ! -s "$diff" ]; then
        diff_one_commit HEAD > "$diff"
        shape=commit
        head_label=HEAD
        resolved_by="git show HEAD"
        resolution_step=auto-3-head
        if [ ! -s "$diff" ]; then
          FELL_THROUGH+=("auto-3-head: HEAD is an empty commit")
          die "nothing to review. $(printf '%s; ' "${FELL_THROUGH[@]}")Name a scope -- a ref, a range, a PR number or a path."
        fi
      fi
      ;;
  esac

  # The invariant. An empty artifact is the one thing a consumer must never be handed,
  # because a collapsed range and a genuinely empty change look identical on disk.
  if [ ! -s "$diff" ]; then
    die "--scope $scope resolved to an empty diff. That is not a verified absence of changes -- a collapsed range looks the same. Name a scope that has one."
  fi

  local workspace_head workspace_dirty=false workspace_untracked=false rc
  workspace_head="$(git rev-parse HEAD)"
  # An error from `git diff --quiet` must not read as clean, so the exit code is captured
  # and the three outcomes are separated rather than collapsed into a boolean.
  rc=0; git diff --quiet HEAD || rc=$?
  case $rc in
    0) ;;
    1) workspace_dirty=true ;;
    *) die "git diff --quiet HEAD failed (exit $rc); the state of the working tree is unknown" ;;
  esac
  [ -z "$(git ls-files --others --exclude-standard)" ] || workspace_untracked=true

  # Direction matters, so the ancestor case is two states rather than one: reviewing HEAD~3
  # leaves the checkout carrying commits the review does not cover, while reviewing an
  # unmerged branch means the reviewed commits are not checked out at all. Those call for
  # opposite disclosures. `unknown` is its own answer for a PR head that was never fetched
  # -- `divergent` there would report a comparison that was never made.
  local correspondence note
  if [ "$shape" = worktree ] || [ "$shape" = path ]; then
    correspondence=workspace
    note="the scope is the working tree itself, so uncommitted state is the subject rather than a contaminant"
  elif [ -z "$scope_head" ] || ! git rev-parse --verify --quiet "$scope_head^{commit}" >/dev/null 2>&1; then
    correspondence=unknown
    note="the scope's head is not present locally, so no comparison with the checkout was possible; read the code from the diff rather than from disk"
  elif [ "$scope_head" = "$workspace_head" ] && [ "$workspace_dirty" = false ]; then
    correspondence=same
    note="the checkout holds exactly the code under review"
  elif [ "$scope_head" = "$workspace_head" ]; then
    correspondence=same
    note="the checkout is at the reviewed commit but carries uncommitted edits, so a file on disk may not match the diff"
  else
    rc=0; git merge-base --is-ancestor "$scope_head" "$workspace_head" || rc=$?
    case $rc in
      0) correspondence="scope-behind"
         note="the checkout is ahead of what is under review; a file read at its current contents may not match the diff" ;;
      # Only exit 1 means "not an ancestor". Anything above it means the question could not
      # be asked, which must not be reported as an answer.
      1) rc=0; git merge-base --is-ancestor "$workspace_head" "$scope_head" || rc=$?
         case $rc in
           0) correspondence="scope-ahead"
              note="the reviewed commits are not checked out; read them with git show <scope_head>:<path> rather than from disk" ;;
           1) correspondence=divergent
              note="the checkout and the scope are on different lines of history; read files with git show <scope_head>:<path>, never from disk" ;;
           *) correspondence=unknown
              note="the two heads could not be compared" ;;
         esac ;;
      *) correspondence=unknown
         note="the two heads could not be compared" ;;
    esac
  fi

  local file_count
  files_from_diff "$diff" "$tmp/files.json"
  file_count="$(jq length < "$tmp/files.json")"
  [ "$file_count" -gt 0 ] \
    || die "the diff resolved but no files could be read from it; refusing to write a manifest that would claim an empty scope"

  # Composed once, here, so that eight lenses and a merged report describe one scope in one
  # form rather than nine.
  local scope_line
  scope_line="$resolved_by — $file_count files"
  [ "$resolution_step" = explicit ] || scope_line="$scope_line ($resolution_step, nothing named)"
  scope_line="$scope_line; $note"

  local diff_sha256 diff_bytes
  diff_sha256="$(shasum -a 256 "$diff" | cut -d' ' -f1)"
  diff_bytes="$(wc -c < "$diff" | tr -d ' ')"

  # jq builds the JSON: a path with a quote, a backslash or a newline in it cannot break a
  # manifest it escaped, and hand-rolled escaping here would be one more thing to get wrong.
  jq -n \
    --arg scope_arg "$scope" \
    --arg shape "$shape" \
    --arg resolution_step "$resolution_step" \
    --arg resolved_by "$resolved_by" \
    --arg scope_line "$scope_line" \
    --arg base_ref "$base" \
    --arg base_sha "$base_sha" \
    --arg scope_head "$scope_head" \
    --arg head_label "$head_label" \
    --arg workspace_head "$workspace_head" \
    --argjson workspace_dirty "$workspace_dirty" \
    --argjson workspace_untracked "$workspace_untracked" \
    --arg correspondence "$correspondence" \
    --arg correspondence_note "$note" \
    --argjson default_branch_resolved "$base_resolved" \
    --arg base_stale_reason "$FETCH_WARNING" \
    --arg out_dir "$out" \
    --arg diff_sha256 "$diff_sha256" \
    --argjson diff_bytes "$diff_bytes" \
    --argjson file_count "$file_count" \
    --arg file_list_source "$FILE_LIST_SOURCE" \
    --slurpfile files "$tmp/files.json" \
    --argjson fell_through "$(json_array_from_lines ${FELL_THROUGH[@]+"${FELL_THROUGH[@]}"})" \
    --argjson warnings "$(json_array_from_lines ${WARNINGS[@]+"${WARNINGS[@]}"})" \
    '{
      scope_arg: (if $scope_arg == "" then null else $scope_arg end),
      shape: $shape,
      resolution_step: $resolution_step,
      resolved_by: $resolved_by,
      scope_line: $scope_line,
      fell_through: $fell_through,
      base_ref: (if $base_ref == "" then null else $base_ref end),
      base_sha: (if $base_sha == "" then null else $base_sha end),
      default_branch_resolved: $default_branch_resolved,
      base_stale: ($base_stale_reason != ""),
      base_stale_reason: (if $base_stale_reason == "" then null else $base_stale_reason end),
      scope_head: (if $scope_head == "" then null else $scope_head end),
      head_label: $head_label,
      workspace_head: $workspace_head,
      workspace_dirty: $workspace_dirty,
      workspace_untracked: $workspace_untracked,
      correspondence: $correspondence,
      correspondence_note: $correspondence_note,
      out_dir: $out_dir,
      diff_path: ($out_dir + "/scope.diff"),
      diff_bytes: $diff_bytes,
      diff_sha256: $diff_sha256,
      file_count: $file_count,
      file_list_source: $file_list_source,
      files: $files[0],
      warnings: $warnings
    }' > "$tmp/manifest.json"
  rm -f "$tmp/files.json"

  rm -rf "$out"
  mkdir -p "$(dirname "$out")"
  mv "$tmp" "$out"
  trap - EXIT

  echo "$scope_line"
  [ ${#WARNINGS[@]} -eq 0 ] || printf 'warning: %s\n' "${WARNINGS[@]}" >&2
  [ -z "$FETCH_WARNING" ] || echo "warning: $FETCH_WARNING" >&2
  echo "$out"
}

cmd_path() {
  local scope=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --scope) scope="${2?--scope needs a value}"; shift 2 ;;
      *) die "unknown argument: $1" ;;
    esac
  done
  scope_out_dir "$scope" "$(repo_root)"
}

case "${1:-}" in
  resolve) shift; cmd_resolve "$@" ;;
  base)
    repo_root >/dev/null
    resolve_default_branch || die "cannot resolve a default branch; ask for one, or pass --base"
    ;;
  path) shift; cmd_path "$@" ;;
  *) die "usage: $0 resolve [--scope <ref|range|path|PR#>] [--base <ref>] [--no-fetch] | base | path [--scope <arg>]" ;;
esac
