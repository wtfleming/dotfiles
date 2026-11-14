#!/usr/bin/env bash
# Exercise fix-snapshot.sh against a scratch repository.
#
# Its failure mode is a fix diff that silently omits a file -- an untracked one, a deleted
# one -- so the cold review of a fix round reads less than was written and says nothing.

set -Eeuo pipefail

SNAP="$(cd "$(dirname "$0")/.." && pwd)/scripts/fix-snapshot.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export TMPDIR="$WORK"
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

git init -q -b main "$WORK/repo"
cd "$WORK/repo"
git config user.email ci@example.invalid
git config user.name CI
printf 'one\n' > tracked.txt
printf 'ignored.txt\n' > .gitignore
git add . && git commit -q -m base

# The change under review: a tracked edit and two untracked files.
printf 'reviewed\n' >> tracked.txt
printf 'reviewed\n' > untracked.txt
printf 'reviewed\n' > doomed.txt
status_before="$(git status --porcelain)"
tree="$("$SNAP" take)"
check "take leaves the index and tree alone" "$status_before" "$(git status --porcelain)"
check "an untouched tree diffs as empty" "empty" "$("$SNAP" diff "$tree")"

# The fixes.
printf 'fix\n' >> tracked.txt
printf 'fix\n' >> untracked.txt
rm doomed.txt
printf 'fix\n' > created.txt
printf 'noise\n' > ignored.txt
diff="$("$SNAP" diff "$tree")"
check "the files the fixes touched, and only those" \
  "created.txt doomed.txt tracked.txt untracked.txt" \
  "$(git apply --numstat "$diff" | cut -f3 | sort | tr '\n' ' ' | sed 's/ $//')"
check "the reviewed change is not in the fix diff" "0" "$(grep -c '^+reviewed' "$diff" || :)"
check "a file the fixes deleted is a deletion" "1" "$(grep -c '^deleted file' "$diff" || :)"

if [ "$failures" -gt 0 ]; then
  echo "$failures check(s) failed" >&2
  exit 1
fi
echo "all checks passed"
