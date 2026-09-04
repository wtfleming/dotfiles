#!/usr/bin/env bash
# Exercise resolve-scope.sh against a scratch repository.
#
# Everything else CI runs is static -- shellcheck, `jq empty`, a frontmatter parse -- and
# none of it can reach the branching this script is almost entirely made of. That branching
# is now the single authority on what every review, verification and design pass reads, so
# a wrong answer here is a confident report about the wrong code.
#
# Each case asserts the manifest fields the consumers actually branch on. `correspondence`
# is the highest-value one: every agent decides from it whether to read a file from disk.

set -Eeuo pipefail

RESOLVE="$(cd "$(dirname "$0")/.." && pwd)/scripts/resolve-scope.sh"
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

# `git init -b` needs git 2.28+; the runner has it, and so does the macOS target.
scratch_repo() {
  local dir=$1 branch=${2:-main}
  git init -q -b "$branch" "$dir"
  git -C "$dir" config user.email ci@example.invalid
  git -C "$dir" config user.name CI
}

commit() {
  local dir=$1 file=$2 body=$3 msg=$4
  printf '%s\n' "$body" > "$dir/$file"
  git -C "$dir" add "$file"
  git -C "$dir" commit -q -m "$msg"
}

field() {
  jq -r "$2" "$1/manifest.json"
}

echo "== default branch is resolved, not assumed =="
scratch_repo "$WORK/trunk" trunk
commit "$WORK/trunk" a.txt one base
check "base on a trunk repo" "trunk" "$(cd "$WORK/trunk" && "$RESOLVE" base)"

echo "== the three auto fall-through steps =="
scratch_repo "$WORK/auto"
commit "$WORK/auto" a.txt one base
printf 'dirty\n' >> "$WORK/auto/a.txt"
out=$(cd "$WORK/auto" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "auto step 1 shape" "worktree" "$(field "$out" .shape)"
check "auto step 1 step" "auto-1-worktree" "$(field "$out" .resolution_step)"
check "auto step 1 correspondence" "workspace" "$(field "$out" .correspondence)"

git -C "$WORK/auto" checkout -q -- a.txt
git -C "$WORK/auto" checkout -q -b feature
commit "$WORK/auto" b.txt two "on the branch"
out=$(cd "$WORK/auto" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "auto step 2 step" "auto-2-branch" "$(field "$out" .resolution_step)"
check "auto step 2 records step 1" "1" "$(field "$out" '.fell_through | length')"

echo "== untracked work is folded into the same diff =="
scratch_repo "$WORK/untracked"
commit "$WORK/untracked" a.txt one base
printf 'new\n' > "$WORK/untracked/fresh.txt"
out=$(cd "$WORK/untracked" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "untracked file is in the scope" "fresh.txt" "$(field "$out" '.files[0]')"

echo "== ranges split on the operator, not the last dot =="
scratch_repo "$WORK/range"
commit "$WORK/range" a.txt one base
git -C "$WORK/range" tag v1.0
commit "$WORK/range" a.txt two second
git -C "$WORK/range" tag v2.0
head=$(git -C "$WORK/range" rev-parse HEAD)
for spec in 'v1.0..v2.0' 'v1.0...v2.0' 'HEAD~1..'; do
  out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope "$spec" 2>/dev/null | tail -1)
  check "range $spec resolves its head" "$head" "$(field "$out" .scope_head)"
done

echo "== a commit scope is one commit, and HEAD is a commit =="
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "HEAD is a commit, not a branch" "commit" "$(field "$out" .shape)"

echo "== correspondence tracks the checkout =="
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD~1 2>/dev/null | tail -1)
check "an older commit is scope-behind" "scope-behind" "$(field "$out" .correspondence)"
printf 'uncommitted\n' >> "$WORK/range/a.txt"
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "a dirty checkout is same-dirty" "same-dirty" "$(field "$out" .correspondence)"
git -C "$WORK/range" checkout -q -- a.txt

echo "== a root commit has no parent to diff against =="
scratch_repo "$WORK/root"
commit "$WORK/root" a.txt one base
out=$(cd "$WORK/root" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "root commit resolves" "1" "$(field "$out" .file_count)"

echo "== a scope may not be an option, and prose is not a failure =="
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope '--output=/tmp/pwned' >/dev/null 2>&1) || rc=$?
check "an option-shaped scope is refused" "1" "$rc"
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'how login works' >/dev/null 2>&1) || rc=$?
check "prose exits 2" "2" "$rc"
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'no-such-branch' >/dev/null 2>&1) || rc=$?
check "an unresolvable token exits 1" "1" "$rc"

echo "== an empty scope is never written =="
scratch_repo "$WORK/empty"
commit "$WORK/empty" a.txt one base
rc=0; (cd "$WORK/empty" && "$RESOLVE" resolve --scope main >/dev/null 2>&1) || rc=$?
check "standing on the default branch is a stop, not an empty manifest" "1" "$rc"

if [ "$failures" -gt 0 ]; then
  echo "$failures check(s) failed" >&2
  exit 1
fi
echo "all checks passed"
