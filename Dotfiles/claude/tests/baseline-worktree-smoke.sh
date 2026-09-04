#!/usr/bin/env bash
# Exercise baseline-worktree.sh against a scratch repository.
#
# This script builds and destroys git worktrees on paths derived from $TMPDIR, and its
# failure modes are the expensive kind: a guard that does not stop the run, a stale
# registration nothing clears, half a pair left behind for the next run to trip over.
# None of that is reachable by shellcheck, and none of it was covered until now.
#
# Every case runs with its own $TMPDIR, so the worktree parent one case poisons cannot
# reach the next. No package manager is involved: the scratch repo detects no ecosystem,
# and where a bootstrap has to fail it is forced with --build-cmd.

set -Eeuo pipefail

SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/skills/wtf-code-verify/scripts/baseline-worktree.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
failures=0

check() {
  local label=$1 expected=$2 actual=$3
  if [ "$expected" = "$actual" ]; then
    echo "  ok    $label"
  else
    echo "  FAIL  $label: expected '$expected', got '$actual'" >&2
    failures=$((failures + 1))
  fi
}

# A repo with a baseline commit, two more on main, and a feature branch off the baseline.
# --base names the baseline explicitly: left to its own devices the script would pick the
# default branch, and the merge base would collapse onto HEAD.
scratch_repo() {
  local root=$1
  mkdir -p "$root"
  git -C "$root" init -q -b main
  git -C "$root" config user.email t@example.com
  git -C "$root" config user.name test
  printf 'one\n' > "$root/a.txt"
  git -C "$root" add a.txt && git -C "$root" commit -q -m base
  BASE_SHA="$(git -C "$root" rev-parse HEAD)"
  printf 'two\n' > "$root/b.txt"
  git -C "$root" add b.txt && git -C "$root" commit -q -m second
  git -C "$root" checkout -q -b feature "$BASE_SHA"
  printf 'marker\n' > "$root/marker"
  git -C "$root" add marker && git -C "$root" commit -q -m feature
  git -C "$root" checkout -q main
}

REPO="$WORK/repo"
scratch_repo "$REPO"

# Each case gets a parent of its own, and `run` keeps the exit status rather than letting
# set -e take the suite down with the script under test.
run() {
  local tmp=$1; shift
  local rc=0
  ( cd "$REPO" && TMPDIR="$tmp" "$SCRIPT" "$@" ) >/dev/null 2>&1 || rc=$?
  echo "$rc"
}
nonzero() {
  [ "$1" != 0 ] && echo nonzero || echo zero
}
run_out() {
  local tmp=$1; shift
  ( cd "$REPO" && TMPDIR="$tmp" "$SCRIPT" "$@" ) 2>/dev/null || true
}

echo "== path and remove are queries, and create nothing =="
Q="$WORK/tmp-query"; mkdir -p "$Q"
out="$(run_out "$Q" path baseline)"
check "path answers" "$Q/verify-baseline/repo" "$out"
check "and creates nothing" "no" "$([ -e "$Q/verify-baseline" ] && echo yes || echo no)"
check "remove reports nothing to remove" "0" "$(run "$Q" remove)"
check "and still creates nothing" "no" "$([ -e "$Q/verify-baseline" ] && echo yes || echo no)"

echo "== a worktree parent somebody else could have planted is refused =="
H="$WORK/tmp-hostile"; mkdir -p "$H" "$WORK/hostile-target"
ln -s "$WORK/hostile-target" "$H/verify-baseline"
# The refusal has to *stop* the run. Reported through a command substitution it would
# exit that subshell alone, and create would carry on with a truncated path.
check "create refuses a symlinked parent" "1" "$(run "$H" create --base "$BASE_SHA")"
check "and nothing was built through it" "" "$(ls -A "$WORK/hostile-target")"
# The query is not guarded, because it writes nothing: it still answers.
check "path still answers under it" "$H/verify-baseline/repo" "$(run_out "$H" path baseline)"

# Ownership alone would accept this: the directory is ours, and only the mode is wrong.
# A group- or world-writable parent lets another local user swap the tree between
# `git worktree add` and the bootstrap that runs its package manager.
echo "== a worktree parent we own but anyone can write to is refused =="
for mode in 777 770; do
  W="$WORK/tmp-mode$mode"; mkdir -p "$W/verify-baseline"; chmod "$mode" "$W/verify-baseline"
  check "create refuses a $mode parent" "1" "$(run "$W" create --base "$BASE_SHA")"
done
# ...and the same directory at 700 is accepted, so the check is about the mode and not
# about the directory existing.
W="$WORK/tmp-mode700"; mkdir -p "$W/verify-baseline"; chmod 700 "$W/verify-baseline"
check "and a 700 parent is not refused for its mode" "0" \
  "$(run "$W" create --base "$BASE_SHA")"
run "$W" remove >/dev/null 2>&1 || true

# The leaf check alone misses this: verify-baseline is 700 and its parent is not, so
# another local user can rename the parent and substitute a tree after the check has
# passed. Sticky is the exception that keeps /tmp usable -- without `t`, anyone may
# rename anyone else's entries; with it, only the owner can.
echo "== an ancestor of the worktree parent that others can rename is refused =="
A="$WORK/anc"; mkdir -p "$A/verify-baseline"; chmod 700 "$A/verify-baseline"
chmod 777 "$A"
check "create refuses a 777 non-sticky ancestor" "1" "$(run "$A" create --base "$BASE_SHA")"
chmod 1777 "$A"
check "and accepts the same ancestor when it is sticky" "0" "$(run "$A" create --base "$BASE_SHA")"
run "$A" remove >/dev/null 2>&1 || true

echo "== a registration whose directory is gone does not block the next create =="
P="$WORK/tmp-purge"; mkdir -p "$P"
check "the pair is created" "0" "$(run "$P" create --head feature --base "$BASE_SHA")"
# What a $TMPDIR purge leaves behind: the registrations survive in .git/worktrees with
# no directory on disk. git records them resolved, so a literal string comparison
# against the composed path never matches and nothing ever clears them.
rm -rf "$P/verify-baseline"
check "both registrations are still there" "2" \
  "$(git -C "$REPO" worktree list --porcelain | grep -c 'verify-baseline')"
check "and create recovers rather than dying on them" "0" \
  "$(run "$P" create --head feature --base "$BASE_SHA")"
check "cleanup" "0" "$(run "$P" remove)"

echo "== half a pair is refused rather than reused =="
I="$WORK/tmp-incomplete"; mkdir -p "$I"
check "the pair is created" "0" "$(run "$I" create --head feature --base "$BASE_SHA")"
rm -rf "$I/verify-baseline/repo-head"
check "one half missing is a stop" "1" "$(run "$I" create --head feature --base "$BASE_SHA")"
check "and --force recreates both" "0" "$(run "$I" create --force --head feature --base "$BASE_SHA")"
check "cleanup" "0" "$(run "$I" remove)"

echo "== a bootstrap that fails on the head side leaves no lone baseline =="
B="$WORK/tmp-unwind"; mkdir -p "$B"
# Passes on the baseline commit and fails on the feature commit, which is the only one
# carrying `marker` -- so the baseline tree is built and the head tree's build fails.
check "the run fails" "1" \
  "$(run "$B" create --head feature --base "$BASE_SHA" --build-cmd 'test ! -f marker')"
check "the baseline was unwound too" "no" \
  "$([ -d "$B/verify-baseline/repo" ] && echo yes || echo no)"
check "and left no registration behind" "0" \
  "$(git -C "$REPO" worktree list --porcelain | grep -c 'verify-baseline' || true)"

echo "== the same, when the head tree fails before its own trap is armed =="
A="$WORK/tmp-early"; mkdir -p "$A/verify-baseline"
# A plain file where the head worktree goes: `git worktree add` refuses it, which fails
# inside add_tree *before* it installs a trap of its own. Only the caller's trap can
# unwind the baseline here -- and it runs with add_tree's locals in scope, so a shadowed
# variable there destroys the wrong tree and leaves the baseline registered.
printf 'in the way\n' > "$A/verify-baseline/repo-head"
check "the run fails" "nonzero" "$(nonzero "$(run "$A" create --head feature --base "$BASE_SHA")")"
check "the baseline was unwound" "no" \
  "$([ -d "$A/verify-baseline/repo" ] && echo yes || echo no)"
check "and left no registration behind" "0" \
  "$(git -C "$REPO" worktree list --porcelain | grep -c 'verify-baseline' || true)"

echo
if [ "$failures" -eq 0 ]; then
  echo "all checks passed"
else
  echo "$failures check(s) failed" >&2
  exit 1
fi
