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
# authentication"). There is no diff to produce. Prose exits 2 -- a distinct code, so a
# caller can branch to its subject path without parsing an error string. A single token
# that is simply not a ref or a path is a caller error and exits 1, because a typo'd
# branch name routed into the subject procedure returns a confident review of a scope
# nobody asked for.
#
# Written for bash 3.2 (the macOS system bash), so no associative arrays, no mapfile.
#
# Usage:
#   resolve-scope.sh resolve [--scope <ref|range|path|PR#|PR-url>] [--base <ref>]
#   resolve-scope.sh base

set -Eeuo pipefail

# Exit 2 is a contract: it means "the scope is prose, run the subject procedure", and a
# caller that reads it will happily review an area of behaviour named after a real commit.
# jq exits 2 on a system error of its own -- a truncated or missing --slurpfile input, a
# full $TMPDIR -- and under `set -e` that status would become this script's. Anything that
# is not the deliberate `exit 2` below leaves as 1.
SUBJECT_EXIT=0
trap 'rc=$?; if [ "$rc" -eq 2 ] && [ "$SUBJECT_EXIT" -ne 1 ]; then rc=1; fi; exit "$rc"' ERR

WARNINGS=()
FELL_THROUGH=()
FETCH_WARNING=""
FILE_LIST_SOURCE=git-apply-numstat
SHAPE=""
PR_NUMBER=""
CORRESPONDENCE=""
CORRESPONDENCE_NOTE=""

# Set by resolve_diff_for_shape: the commit the diff is of, a human label for it, the
# command that produced it, and which fall-through step settled an unnamed scope.
SCOPE_HEAD=""
HEAD_LABEL=""
RESOLVED_BY=""
RESOLUTION_STEP=explicit

# Untracked files above this go into the diff as a stub rather than inline. A single
# untracked 200MB CSV would otherwise become a 200MB scope.diff that eight agents are
# each told to read in full.
MAX_INLINE_BYTES=1048576
# And a budget across all of them, because the per-file cap alone does not bound the total:
# four hundred files of just under the per-file limit clear every check individually and
# still build a diff nobody can read. Ten times the per-file cap -- high enough that
# ordinary untracked work never trips it, low enough to bite well before eight agents'
# context does.
MAX_UNTRACKED_TOTAL_BYTES=10485760
# Past this many untracked files the per-file `git diff` calls dominate the run, so say so
# rather than appearing to hang.
UNTRACKED_NOISY_COUNT=500

warn() {
  WARNINGS+=("$*")
}

flush_warnings() {
  [ ${#WARNINGS[@]} -eq 0 ] || printf 'warning: %s\n' "${WARNINGS[@]}" >&2
  [ -z "$FETCH_WARNING" ] || echo "warning: $FETCH_WARNING" >&2
}

# Flush what was collected before exiting. Without this the diagnostic that explains a
# failure is printed on the success path and discarded on the failure path -- exactly
# backwards, since the failure is when a reader needs it.
die() {
  flush_warnings
  echo "error: $*" >&2
  exit 1
}

# Resolves the main checkout even when invoked from inside a linked worktree, where
# --show-toplevel returns the enclosing worktree. Same reason as baseline-worktree.sh.
repo_root() {
  git rev-parse --git-common-dir >/dev/null 2>&1 || die "not inside a git repository"
  dirname "$(git rev-parse --path-format=absolute --git-common-dir)"
}

# owner/repo for the origin remote, from the local config -- no network. Handles the scp
# form (git@host:o/r), the URL forms, and a trailing .git.
# Empty output rather than a failure when there is no origin: this runs inside a command
# substitution, so a non-zero return would take `set -e` with it and exit carrying git's
# own status -- which for a missing remote is 2, the code callers read as "this is a
# subject". The caller checks for empty and reports the real cause.
remote_slug() {
  local url
  url="$(git remote get-url origin 2>/dev/null)" || return 0
  printf '%s' "$url" | sed -e 's|\.git$||' -e 's|^.*://[^/]*/||' -e 's|^.*:||'
}

# Guarded like jq and gh below: this runs before either of their checks, so an image
# without perl died on a bare `shasum: command not found` and exit 127.
hash12() {
  printf '%s' "$1" | sha256_stdin | cut -c1-12
}

# One place, because there are two callers and a bare `shasum` in either dies with
# `command not found` and exit 127 rather than a named cause.
sha256_stdin() {
  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256sum
  else
    die "neither shasum nor sha256sum is installed; one is needed to hash the scope"
  fi
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
# failure is recorded and disclosed instead of raised.
#
# Bounded, because "non-fatal" is not the same as "non-blocking": with --quiet and stderr
# discarded, a remote that black-holes SYNs would otherwise stall every review for the OS
# TCP timeout -- 75 seconds, silently, before any agent is dispatched.
# gh has no client timeout of its own, so a connection that establishes and then goes
# silent would hang here for as long as the peer holds it open. `timeout` is GNU and
# `gtimeout` is its Homebrew name; where neither is installed the call runs unbounded
# rather than not at all -- a missing bound is worse than a missing resolver.
run_bounded() {
  local timeout_bin=""

  if command -v timeout >/dev/null 2>&1; then
    timeout_bin=timeout
  elif command -v gtimeout >/dev/null 2>&1; then
    timeout_bin=gtimeout
  fi

  if [ -n "$timeout_bin" ]; then
    "$timeout_bin" 30 "$@"
  else
    "$@"
  fi
}

fetch_base() {
  local base=$1 remote branch
  case "$base" in
    # A local name is only ever chosen when no remote-tracking ref resolved, so there is no
    # remote for it to be behind. Warning there would put a staleness note on every review
    # in a local-only repo, which spends the credibility of the real one.
    */*) remote="${base%%/*}"; branch="${base#*/}" ;;
    *)   return 0 ;;
  esac
  # Three bounds, because none of them covers the others. ConnectTimeout stops a
  # remote that black-holes SYNs, but only during connection setup; the HTTP
  # low-speed pair stops a transfer that stalls, but only over HTTP; and neither
  # bounds an ssh session that connects and then goes quiet. run_bounded is the
  # only one that holds whatever the transport does.
  if ! run_bounded env GIT_TERMINAL_PROMPT=0 \
       GIT_SSH_COMMAND='ssh -o ConnectTimeout=5 -o BatchMode=yes -o ServerAliveInterval=5 -o ServerAliveCountMax=2' \
       GIT_HTTP_LOW_SPEED_LIMIT=1000 GIT_HTTP_LOW_SPEED_TIME=10 \
       git fetch --quiet "$remote" "$branch" 2>/dev/null; then
    FETCH_WARNING="git fetch $remote $branch failed; the merge base may be behind $remote"
  fi
}

# Keyed on the repo *and* the scope. The repo half carries a hash of the absolute path
# because two checkouts can share a basename -- for baseline-worktree.sh that collision is
# loud, since `git worktree add` refuses an existing directory, but here it would silently
# hand one repo's diff to the other's agents. The scope half keeps two scopes in one repo
# from overwriting each other.
scope_out_dir() {
  local scope=$1 root=$2 tmp slug
  tmp="${TMPDIR:-/tmp}"
  slug="$(printf '%s' "${scope:-auto}" | tr -c 'A-Za-z0-9._-' '-' | cut -c1-40)"
  echo "${tmp%/}/wtf-scope/$(basename "$root")-$(hash12 "$root")/${slug}-$(hash12 "${scope:-auto}")"
}

# Sets SHAPE, and PR_NUMBER for a pull request. Assigns to globals rather than echoing,
# because `shape="$(scope_shape_of ...)"` runs in a subshell, and every `warn` raised in
# here would be appended to a copy of WARNINGS that is discarded when it returns.
#
# `scope` has already been rewritten to a repo-relative path by the caller where it named
# one. Order matters: a PR before a ref because bare digits almost never name one, a ref
# before a path because that is git's own convention, a path last.
scope_shape_of() {
  local scope=$1 full is_ref url_slug here_slug
  [ -n "$scope" ] || { SHAPE=worktree; return 0; }

  # A leading dash reaches `git diff` in option position, where `--output=<path>` truncates
  # and overwrites that path. The scope is not always the user's own typing -- a parent
  # agent composes it after reading the tree under review -- so this is refused outright.
  case "$scope" in
    -*) die "a scope may not begin with '-': $scope" ;;
  esac

  case "$scope" in
    */pull/[0-9]*)
      # A URL names a repository as well as a number, and dropping that half is how
      # `https://github.com/someone-else/repo/pull/123` becomes `gh pr diff 123` against
      # *this* repo -- publishing an unrelated PR's diff while reporting the URL as its
      # scope. Everything downstream (the merge base, the correspondence, the blob reads)
      # is local anyway, so a foreign PR has no meaning here and is refused rather than
      # retargeted.
      SHAPE="pr"
      PR_NUMBER="${scope##*/pull/}"; PR_NUMBER="${PR_NUMBER%%/*}"
      url_slug="${scope#*://}"; url_slug="${url_slug#*/}"; url_slug="${url_slug%%/pull/*}"
      here_slug="$(remote_slug)"
      [ -n "$here_slug" ] \
        || die "cannot tell which repository this checkout is; pass the PR number rather than a URL"
      [ "$(printf '%s' "$url_slug" | tr '[:upper:]' '[:lower:]')" \
        = "$(printf '%s' "$here_slug" | tr '[:upper:]' '[:lower:]')" ] \
        || die "that PR URL is for $url_slug, but this checkout is $here_slug. Review it from a checkout of that repository."
      return 0 ;;
    '#'[0-9]*)     SHAPE="pr"; PR_NUMBER="${scope#\#}"; return 0 ;;
    ''|*[!0-9]*)   ;;
    *)             SHAPE="pr"; PR_NUMBER="$scope"; return 0 ;;
  esac

  case "$scope" in
    # HEAD names one commit, and the auto fall-through's third step reads it as exactly
    # that. Left to the symbolic-name test below it resolves to refs/heads/<branch> and is
    # read as the whole branch, so one commit would mean two scopes depending on whether it
    # was spelled HEAD or HEAD~0.
    HEAD|@) SHAPE=commit; return 0 ;;
    *..*)   SHAPE=range;  return 0 ;;
  esac

  # Matched against refs/, not merely tested for emptiness: `rev-parse
  # --symbolic-full-name` echoes an argument it does not recognise straight back, so a bare
  # `-n` test reads every path as a ref and sends it to be diffed as a commit.
  full="$(git rev-parse --symbolic-full-name "$scope" 2>/dev/null || true)"
  case "$full" in
    refs/*) is_ref=true ;;
    *)      is_ref=false ;;
  esac
  if [ "$is_ref" = true ] || git rev-parse --verify --quiet "$scope^{commit}" >/dev/null 2>&1; then
    # The collision is disclosed once, here, before the split below -- a branch that shares
    # a name with a file is the same ambiguity as a tag that does, and warning on only one
    # of them means the quieter case is the one nobody hears about.
    [ -e "$scope" ] && warn "'$scope' is both a ref and a path; read as a ref (pass ./$scope for the path)"
    case "$full" in
      # A branch means "against its merge base"; any other commit-ish means that commit
      # alone. Conflating them reviews one commit of a branch that has many.
      refs/heads/*|refs/remotes/*) SHAPE=branch; return 0 ;;
    esac
    SHAPE=commit
    return 0
  fi
  [ -e "$scope" ] && { SHAPE=path; return 0; }

  # Prose and a typo need different answers, and one exit code cannot carry both. Callers
  # treat exit 2 as "this is a subject" and run the subject procedure; a mistyped branch
  # name sent down that path returns a confident review of a scope nobody asked for.
  case "$scope" in
    *[[:space:]]*)
      cat >&2 <<EOF
error: not a PR, a range, a ref or an existing path: $scope
  Read as a subject -- prose naming an area of behaviour. There is no diff to resolve and
  this is the wrong tool; the caller classifies subjects itself.
EOF
      SUBJECT_EXIT=1
      exit 2 ;;
  esac
  die "'$scope' resolves to neither a ref nor a path. Check the spelling, or fetch the branch first. (Prose naming an area of behaviour is a subject, and exits 2.)"
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
  local out=$1 filter=$2 f rc size count=0 total=0 over_budget=false noted=false

  # Straight to stderr, and before the walk rather than after it. This exists to explain
  # a wait -- two processes per file -- and `warn` only queues a line for the flush that
  # happens once the manifest is written, which is after the slow part is over.
  count="$(git ls-files --others --exclude-standard -z ${filter:+-- "$filter"} | tr -cd '\0' | wc -c | tr -d ' ')"
  if [ "${count:-0}" -gt "$UNTRACKED_NOISY_COUNT" ]; then
    echo "note: folding $count untracked files into the diff; this takes a moment." >&2
    noted=true
  fi

  count=0

  while IFS= read -r -d '' f; do
    count=$((count + 1))

    # Skipped once the budget is gone: past that point the file is stubbed whatever its
    # size, so the probe buys nothing but a process per file.
    if [ "$over_budget" = true ]; then
      size=0
    else
      size="$(wc -c < "$f" 2>/dev/null | tr -d ' ')" || size=0
    fi
    if [ "$over_budget" = false ] \
       && [ $((total + ${size:-0})) -gt "$MAX_UNTRACKED_TOTAL_BYTES" ]; then
      over_budget=true
      warn "untracked files passed ${MAX_UNTRACKED_TOTAL_BYTES} bytes in total; the rest are stubbed rather than inlined"
    fi
    if [ "$over_budget" = true ] || [ "${size:-0}" -gt "$MAX_INLINE_BYTES" ]; then
      # The same shape git uses for a binary, so `git apply --numstat` still counts the
      # file and it survives into the file list. Dropping it instead would narrow the
      # scope without saying so.
      {
        printf 'diff --git a/%s b/%s\n' "$f" "$f"
        printf 'new file mode 100644\n'
        printf 'index 0000000..0000000\n'
        printf 'Binary files /dev/null and b/%s differ\n' "$f"
      } >> "$out"
      [ "$over_budget" = true ] \
        || warn "untracked file over ${MAX_INLINE_BYTES} bytes, stubbed rather than inlined: $f ($size bytes)"
      continue
    fi
    total=$((total + ${size:-0}))
    rc=0
    git diff --no-index -- /dev/null "$f" >> "$out" || rc=$?
    # Exit 1 is "they differ", which is every file here. Anything above it is a real
    # failure on one file, and one bad file must not cost the whole scope.
    [ "$rc" -le 1 ] || warn "could not diff untracked file: $f (git exit $rc)"
  done < <(git ls-files --others --exclude-standard -z ${filter:+-- "$filter"})
  if [ "$noted" = true ]; then
    warn "$count untracked files were folded into the diff; consider gitignoring what does not belong in a review"
  fi
}

# `git diff <ref>^!` is shorthand for `<ref>^ <ref>` and has no parent to name on a root
# commit, where the whole tree is the change. The `--` keeps a ref that is also a path from
# aborting the run with git's ambiguous-argument fatal.
diff_one_commit() {
  local ref=$1
  if git rev-parse --verify --quiet "$ref^" >/dev/null 2>&1; then
    git diff "$ref^!" --
  else
    git diff-tree -p --root "$ref" --
  fi
}

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
    warn "git apply could not parse the diff; the file list was recovered from its headers and may be incomplete"
    # Both sides, because a deleted file's `+++` line is /dev/null. Scraping only `+++`
    # reports a change that deletes a source file and edits a README as prose-only, and
    # prose-only is what skips four lenses.
    # `|| :` because grep exits 1 when it selects nothing, which pipefail turns into a
    # silent abort -- on a rename-only or mode-only diff, which is exactly the shape that
    # reaches this fallback, and which the empty-file-list `die` below exists to report.
    sed -n -e 's|^+++ b/||p' -e 's|^--- a/||p' "$diff" \
      | { grep -v '^/dev/null$' || :; } \
      | sort -u \
      | jq -Rs 'split("\n") | map(select(length > 0))' > "$dest"
  fi
  rm -f "$dest.raw"
}

# NUL-delimited into jq rather than split on newlines: a path or a warning containing a
# newline would otherwise arrive as two array elements.
json_array_from_lines() {
  printf '%s\0' "$@" | jq -Rs 'split("\u0000") | map(select(length > 0))'
}

# Sets CORRESPONDENCE and CORRESPONDENCE_NOTE. The note names the real head rather than a
# `<SCOPE_HEAD>` placeholder: it is the one field advertised as ready to paste, it is
# concatenated into `scope_line` verbatim, and on the relay hop where that line is the only
# channel a placeholder leaves the next agent with no head to read the code from.
#
# Direction matters, so the ancestor case is two states rather than one: reviewing HEAD~3
# leaves the checkout carrying commits the review does not cover, while reviewing an
# unmerged branch means the reviewed commits are not checked out at all. Those call for
# opposite disclosures. `unknown` is its own answer for a PR head that was never fetched --
# `divergent` there would report a comparison that was never made. And `same-dirty` is a
# value of its own because the rule every agent follows keys on exactly this field: a
# checkout sitting at the reviewed commit with uncommitted edits does *not* hold the
# reviewed code, and folding it into `same` leaves that state unrouted in the two agents
# that anchor and kill findings.
classify_correspondence() {
  local shape=$1 scope_head=$2 workspace_head=$3 workspace_dirty=$4 rc
  if [ "$shape" = worktree ] || [ "$shape" = path ]; then
    CORRESPONDENCE=workspace
    CORRESPONDENCE_NOTE="the scope is the working tree itself, so uncommitted state is the subject rather than a contaminant"
    return 0
  fi
  if [ -z "$scope_head" ] || ! git rev-parse --verify --quiet "$scope_head^{commit}" >/dev/null 2>&1; then
    CORRESPONDENCE=unknown
    CORRESPONDENCE_NOTE="the scope's head is not present locally, so no comparison with the checkout was possible; read the code from the diff rather than from disk"
    return 0
  fi
  if [ "$scope_head" = "$workspace_head" ]; then
    if [ "$workspace_dirty" = true ]; then
      CORRESPONDENCE="same-dirty"
      CORRESPONDENCE_NOTE="the checkout is at the reviewed commit but carries uncommitted edits, so a file on disk may not match the diff; read with git show $scope_head:<path>"
    else
      CORRESPONDENCE=same
      CORRESPONDENCE_NOTE="the checkout holds exactly the code under review"
    fi
    return 0
  fi
  rc=0; git merge-base --is-ancestor "$scope_head" "$workspace_head" || rc=$?
  case $rc in
    0) CORRESPONDENCE="scope-behind"
       CORRESPONDENCE_NOTE="the checkout is ahead of what is under review; a file read at its current contents may not match the diff" ;;
    # Only exit 1 means "not an ancestor". Anything above it means the question could not be
    # asked, which must not be reported as an answer.
    1) rc=0; git merge-base --is-ancestor "$workspace_head" "$scope_head" || rc=$?
       case $rc in
         0) CORRESPONDENCE="scope-ahead"
            CORRESPONDENCE_NOTE="the reviewed commits are not checked out; read them with git show $scope_head:<path> rather than from disk" ;;
         1) CORRESPONDENCE=divergent
            CORRESPONDENCE_NOTE="the checkout and the scope are on different lines of history; read files with git show $scope_head:<path>, never from disk" ;;
         *) CORRESPONDENCE=unknown
            CORRESPONDENCE_NOTE="the two heads could not be compared" ;;
       esac ;;
    *) CORRESPONDENCE=unknown
       CORRESPONDENCE_NOTE="the two heads could not be compared" ;;
  esac
}

# Resolved and fetched only where it is used. A PR, a range, a single commit, a path and a
# working tree that has uncommitted work all carry their own endpoints, and fetching for
# them buys a network round trip plus a `base_stale` flag that callers are told to disclose
# as "the scope may be wider than the branch" -- a claim that is false when the base cannot
# affect the scope at all.
BASE=""
BASE_SHA=""
BASE_RESOLVED=false
need_base() {
  [ "$BASE_RESOLVED" = true ] && return 0
  if BASE="$(resolve_default_branch)"; then
    BASE_RESOLVED=true
    fetch_base "$BASE"
    BASE_SHA="$(git rev-parse "$BASE")"
    return 0
  fi
  BASE=""
  return 1
}

# Produce the diff for the shape `scope_shape_of` settled on, and record the four
# facts about it that only this step knows.
#
# Lifted out of cmd_resolve, which was 300 lines and is the one function in this file
# that did not read like the rest of it. The three-step fall-through for an unnamed
# scope is the part that most needed a name of its own: it is the only place where
# *not* finding a diff is the normal outcome and the next step is the answer.
#
# Out-values travel as globals, the way SHAPE and CORRESPONDENCE already do, rather
# than as an echoed tuple -- there are four of them and one is a multi-word label.
resolve_diff_for_shape() {
  local scope=$1 diff=$2 mb rhs


  case "$SHAPE" in
    pr)
      command -v gh >/dev/null 2>&1 || die "scope '$scope' is a PR but gh is not installed"
      # gh pr diff is the single authority for a PR, and a failure is a stop rather than a
      # fallback. A locally computed base...head is a *different change*: the local base ref
      # may be stale, the PR may target a non-default base, and the PR may have been
      # rebased. Silently reviewing a near-miss is worse than not reviewing.
      # Bounded for the reason fetch_base is: gh sets no client timeout, so a proxy that
      # accepts the connection and then goes silent hangs the resolver indefinitely,
      # before any agent has been dispatched and with nothing on stderr.
      run_bounded gh pr diff "$PR_NUMBER" > "$diff" \
        || die "gh pr diff $PR_NUMBER failed or timed out. Not falling back to a local diff, which would review something other than the PR."
      SCOPE_HEAD="$(run_bounded gh pr view "$PR_NUMBER" --json headRefOid -q .headRefOid 2>/dev/null || true)"
      HEAD_LABEL="PR #$PR_NUMBER head"
      RESOLVED_BY="gh pr diff $PR_NUMBER"
      ;;
    range)
      git diff "$scope" -- > "$diff"
      # Split on the range operator, not on the last dot: `${scope##*.}` turns v1.0..v2.0
      # into `0`, which resolves to nothing, and the manifest then reports the scope's head
      # as absent while the checkout sits exactly on it.
      local rhs
      case "$scope" in
        *...*) rhs="${scope#*...}" ;;
        *)     rhs="${scope#*..}" ;;
      esac
      [ -n "$rhs" ] || rhs=HEAD
      SCOPE_HEAD="$(git rev-parse --verify --quiet "$rhs^{commit}" 2>/dev/null || true)"
      HEAD_LABEL="$scope"
      RESOLVED_BY="git diff $scope"
      ;;
    commit)
      diff_one_commit "$scope" > "$diff"
      SCOPE_HEAD="$(git rev-parse "$scope^{commit}")"
      HEAD_LABEL="$scope"
      RESOLVED_BY="git diff $scope^!"
      ;;
    branch)
      need_base || die "cannot resolve a default branch to diff '$scope' against; pass --base"
      local mb
      mb="$(git merge-base "$BASE" "$scope")" \
        || die "no merge base between $scope and $BASE"
      BASE_SHA="$mb"
      git diff "$mb...$scope" -- > "$diff"
      SCOPE_HEAD="$(git rev-parse "$scope^{commit}")"
      HEAD_LABEL="$scope"
      RESOLVED_BY="git diff \$(git merge-base $BASE $scope)...$scope"
      ;;
    path)
      git diff HEAD -- "$scope" > "$diff"
      append_untracked "$diff" "$scope"
      SCOPE_HEAD="$(git rev-parse HEAD)"
      HEAD_LABEL="working tree"
      RESOLVED_BY="git diff HEAD -- $scope, plus untracked files under it"
      ;;
    worktree)
      # The three-step order from scope-resolution.md: uncommitted work, else the branch
      # against its merge base, else the commit at HEAD. An empty result at any step means
      # *fall through*, never "no changes" -- so each step that produces nothing records
      # why, and the reasons are carried into the manifest even when a later step succeeds.
      git diff HEAD > "$diff"
      append_untracked "$diff" ""
      SCOPE_HEAD="$(git rev-parse HEAD)"
      HEAD_LABEL="working tree"
      RESOLVED_BY="git diff HEAD, plus untracked files"
      RESOLUTION_STEP=auto-1-worktree

      if [ ! -s "$diff" ]; then
        FELL_THROUGH+=("auto-1-worktree: no uncommitted or untracked changes")
        if need_base; then
          local mb2
          if mb2="$(git merge-base "$BASE" HEAD 2>/dev/null)"; then
            BASE_SHA="$mb2"
            git diff "$mb2...HEAD" > "$diff"
            SHAPE=branch
            HEAD_LABEL=HEAD
            RESOLVED_BY="git diff \$(git merge-base $BASE HEAD)...HEAD"
            RESOLUTION_STEP=auto-2-branch
            [ -s "$diff" ] || FELL_THROUGH+=("auto-2-branch: empty diff against the merge base with $BASE (standing on the default branch, or the branch has no commits of its own)")
          else
            FELL_THROUGH+=("auto-2-branch: no merge base between HEAD and $BASE")
          fi
        else
          FELL_THROUGH+=("auto-2-branch: no default branch resolved")
        fi
      fi

      if [ ! -s "$diff" ]; then
        diff_one_commit HEAD > "$diff"
        SHAPE=commit
        HEAD_LABEL=HEAD
        RESOLVED_BY="git show HEAD"
        RESOLUTION_STEP=auto-3-head
        if [ ! -s "$diff" ]; then
          FELL_THROUGH+=("auto-3-head: HEAD is an empty commit")
          die "nothing to review. $(printf '%s; ' "${FELL_THROUGH[@]}")Name a scope -- a ref, a range, a PR number or a path."
        fi
      fi
      ;;
  esac
}

cmd_resolve() {
  local scope="" base_override=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --scope) scope="${2?--scope needs a value}"; shift 2 ;;
      --base)  base_override="${2:?--base needs a ref}"; shift 2 ;;
      *) die "unknown argument: $1" ;;
    esac
  done

  local root out prefix
  command -v jq >/dev/null 2>&1 \
    || die "jq is required to write the manifest but is not installed (brew install jq)"
  root="$(repo_root)"

  # A relative path scope names something relative to where the caller stands, and the `cd`
  # below would silently re-root it at the repo top -- diffing a same-named directory the
  # caller did not mean, or, where no such name exists up there, failing the -e test and
  # being classified as prose. --show-prefix has to be read before the cd.
  prefix="$(git rev-parse --show-prefix 2>/dev/null || true)"
  cd "$root"
  git rev-parse --verify --quiet HEAD >/dev/null 2>&1 \
    || die "this repository has no commits; there is nothing to resolve a scope against"

  # Only when the bare name is not a ref. Rewriting first would let the caller's cwd
  # overturn the ref-before-path precedence below: the same argument would resolve as a
  # branch from the repo root and as a path from a subdirectory that happens to contain a
  # matching name -- two different reviews, chosen by where the caller stood, silently.
  if [ -n "$prefix" ] && [ -n "$scope" ] && [ -e "$prefix$scope" ]; then
    if git rev-parse --verify --quiet "$scope^{commit}" >/dev/null 2>&1; then
      warn "'$scope' is both a ref and a path under $prefix; read as a ref (pass ./$scope for the path)"
    else
      scope="$prefix$scope"
    fi
  fi

  out="$(scope_out_dir "$scope" "$root")"
  scope_shape_of "$scope"

  if [ -n "$base_override" ]; then
    BASE="$base_override"
    # Fetched before it is validated, and validated before it is used. Setting
    # BASE_RESOLVED short-circuits need_base(), which is where the fetch otherwise lives,
    # so without this an explicit --base origin/main computes its merge base against
    # whatever the last fetch left behind. Fetching *first* matters too: a remote-tracking
    # ref that has never been fetched does not resolve yet, and validating ahead of the
    # fetch rejects a base that is perfectly reachable. fetch_base returns immediately for a
    # local name, so this costs nothing when the base is not a remote-tracking ref.
    fetch_base "$BASE"
    git rev-parse --verify --quiet "$BASE^{commit}" >/dev/null 2>&1 \
      || die "--base $BASE does not resolve to a commit"
    BASE_RESOLVED=true
    BASE_SHA="$(git rev-parse "$BASE")"
  fi

  # Build into a temporary directory and move it into place last, so a consumer that finds a
  # manifest finds a complete scope.diff beside it.
  local tmp="$out.tmp.$$" diff
  rm -rf "$tmp"
  mkdir -p "$tmp"
  trap 'rm -rf "$tmp"' EXIT
  diff="$tmp/scope.diff"
  : > "$diff"
  resolve_diff_for_shape "$scope" "$diff"

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
  # --directory collapses an untracked tree to one entry, which is all an emptiness test
  # needs; --no-empty-directory keeps an empty directory from reading as untracked work.
  [ -z "$(git ls-files --others --exclude-standard --directory --no-empty-directory)" ] \
    || workspace_untracked=true

  classify_correspondence "$SHAPE" "$SCOPE_HEAD" "$workspace_head" "$workspace_dirty"

  local file_count
  files_from_diff "$diff" "$tmp/files.json"
  file_count="$(jq length < "$tmp/files.json")"
  [ "$file_count" -gt 0 ] \
    || die "the diff resolved but no files could be read from it; refusing to write a manifest that would claim an empty scope"

  # Composed once, here, so that eight lenses and a merged report describe one scope in one
  # form rather than nine.
  local scope_line
  scope_line="$RESOLVED_BY — $file_count files"
  [ "$RESOLUTION_STEP" = explicit ] || scope_line="$scope_line ($RESOLUTION_STEP, nothing named)"
  scope_line="$scope_line; $CORRESPONDENCE_NOTE"

  local diff_sha256 diff_bytes
  diff_sha256="$(sha256_stdin < "$diff" | cut -d' ' -f1)"
  diff_bytes="$(wc -c < "$diff" | tr -d ' ')"

  # jq builds the JSON: a path with a quote, a backslash or a newline in it cannot break a
  # manifest it escaped, and hand-rolled escaping here would be one more thing to get wrong.
  jq -n \
    --arg scope_arg "$scope" \
    --arg shape "$SHAPE" \
    --arg resolution_step "$RESOLUTION_STEP" \
    --arg resolved_by "$RESOLVED_BY" \
    --arg scope_line "$scope_line" \
    --arg base_ref "$BASE" \
    --arg base_sha "$BASE_SHA" \
    --arg scope_head "$SCOPE_HEAD" \
    --arg head_label "$HEAD_LABEL" \
    --arg workspace_head "$workspace_head" \
    --argjson workspace_dirty "$workspace_dirty" \
    --argjson workspace_untracked "$workspace_untracked" \
    --arg correspondence "$CORRESPONDENCE" \
    --arg correspondence_note "$CORRESPONDENCE_NOTE" \
    --argjson default_branch_resolved "$BASE_RESOLVED" \
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

  local parent
  parent="$(dirname "$out")"

  # Same reasoning as `umask 077` below, one level up: with TMPDIR unset this lands in a
  # world-writable /tmp, and `mkdir -p` accepts a parent somebody else created. Whoever
  # owns it can replace the manifest and the diff every agent is about to read.
  [ ! -L "$parent" ] || die "$parent is a symlink; refusing to publish a scope under it."
  mkdir -p "$parent"
  [ -O "$parent" ] || die "$parent is not owned by you; refusing to publish a scope under it."

  # Published by rename onto a fresh name, never by deleting the old one: the out dir is
  # keyed on repo and scope, so a second resolve of the same scope would otherwise
  # `rm -rf` the manifest that the agents from the first one are reading by path. The
  # symlink swap is atomic, so a reader holds either the old tree or the new one and
  # never neither.
  local final="$out.$$"
  rm -rf "$final"
  mv "$tmp" "$final"

  # A real directory here predates this scheme; replace it once. `ln -sfn` is what
  # flips the pointer -- NOT `mv` onto $out, which follows an existing symlink and
  # deposits the new link *inside* the old target, leaving readers on the stale tree.
  [ -L "$out" ] || rm -rf "$out"
  ln -sfn "$(basename "$final")" "$out"

  # The swap orphans the previous target, which the old unconditional `rm -rf` used to
  # take with it. Pruned on age rather than immediately, because "not the current
  # target" and "nobody is reading it" are different things: an hour is far longer than
  # any run holds a manifest open, and $TMPDIR is purged on a timer anyway.
  find "$parent" -maxdepth 1 -type d -name "$(basename "$out").*" \
    ! -name "$(basename "$final")" -mmin +60 -exec rm -rf {} + 2>/dev/null || :

  trap - EXIT

  echo "$scope_line"
  flush_warnings
  echo "$out"
}

# scope.diff is the full source of whatever is under review, so it is not left readable by
# other users on the machine. On macOS $TMPDIR is already a per-user 0700 directory; this is
# for the Linux boxes where it is unset and the fallback is a shared /tmp.
umask 077

case "${1:-}" in
  resolve) shift; cmd_resolve "$@" ;;
  base)
    repo_root >/dev/null
    resolve_default_branch || die "cannot resolve a default branch; ask for one, or pass --base"
    ;;
  *) die "usage: $0 resolve [--scope <ref|range|path|PR#|PR-url>] [--base <ref>] | base" ;;
esac
