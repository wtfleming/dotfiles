#!/usr/bin/env bash
# Exercise publish-check.sh.
#
# It is the last thing between a captured credential and a public PR, so its failure
# modes are both silent: a scan that could not run reading as a clean one, and a
# denylist line that matches everything or nothing. The fixtures are assembled from
# halves so this file does not itself hold a string the check would refuse.

set -Eeuo pipefail

SCRIPTS="$(cd "$(dirname "$0")/.." && pwd)/scripts"
CHECK="$SCRIPTS/publish-check.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export PUBLISH_DENYLIST="$WORK/denylist"
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

# Exit status, then stdout, of a check over one file holding $1.
run() {
  local rc=0 out
  printf '%s\n' "$1" > "$WORK/text"
  out=$("$CHECK" "$WORK/text" 2>/dev/null) || rc=$?
  echo "$rc:${out#"$WORK"/}"
}

A20=AAAAAAAAAAAAAAAAAAAA
key="AKIA${A20:0:16}"
token="ghp_${A20}${A20}"
jwt="eyJ${A20}.eyJ${A20}.${A20}"

echo "case: clean text"
check "prose with nothing in it passes" "0:" "$(run 'The second request returns 200 with the updated title.')"
check "a redacted credential passes" "0:" "$(run 'Authorization: Bearer <redacted: SESSION_TOKEN>')"
check "a word that merely contains sk- passes" "0:" "$(run 'the task-runner-for-background-jobs-and-more package')"

echo "case: fixed patterns"
check "an AWS key is refused, by line and name" "1:text:1: AWS access key" "$(run "id=$key")"
check "a GitHub token is refused" "1:text:1: GitHub token" "$(run "$token")"
check "a JSON web token is refused" "1:text:1: JSON web token" "$(run "$jwt")"
check "a bearer value is refused" "1:text:1: Authorization header value" "$(run "Authorization: Bearer ${A20}")"
check "a password in a URL is refused" "1:text:1: password in a URL" "$(run "postgres://app:${A20:0:8}@db/app")"
check "a private key block is refused" "1:text:1: private key block" "$(run "-----BEGIN RSA PRIVATE ""KEY-----")"
check "the line number is the matching line's" "1:text:3: AWS access key" "$(run "one
two
$key")"
out=$(printf '%s\n' "$key" > "$WORK/text"; "$CHECK" "$WORK/text" 2>&1 || true)
check "the matched text is never printed" "0" "$(printf '%s' "$out" | grep -c "$key" || true)"

echo "case: the denylist"
printf '# terms\n\nExampleCorp\ninternal.example\n' > "$PUBLISH_DENYLIST"
check "a listed term is refused, whatever its case" "1:text:1: a term on the denylist" "$(run 'deployed at examplecorp today')"
check "a blank line in the list does not match everything" "0:" "$(run 'nothing listed here')"
printf '# only a comment\n\n' > "$PUBLISH_DENYLIST"
check "a list with no terms checks patterns only" "0:" "$(run 'deployed at examplecorp today')"
rm "$PUBLISH_DENYLIST"
check "no denylist at all is not an error" "0:" "$(run 'deployed at examplecorp today')"
mkdir "$PUBLISH_DENYLIST"
check "a denylist that cannot be read fails closed" "2:" "$(run 'anything')"
rmdir "$PUBLISH_DENYLIST"

echo "case: inputs"
rc=0; "$CHECK" "$WORK/missing" >/dev/null 2>&1 || rc=$?
check "a file that cannot be read is not a clean scan" "2" "$rc"
rc=0; printf '%s\n' "$key" | "$CHECK" - >/dev/null 2>&1 || rc=$?
check "stdin is scanned" "1" "$rc"
rc=0; "$CHECK" >/dev/null 2>&1 || rc=$?
check "no argument is a usage error" "64" "$rc"

echo "case: publish-verify-section.sh refuses a section that fails"
printf 'A body.\n' > "$WORK/body"
printf 'Verified.\n%s\n' "$token" > "$WORK/section"
rc=0; "$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" >/dev/null 2>&1 || rc=$?
check "the merge exits non-zero" "1" "$rc"
check "nothing was written" "no" "$([ -e "$WORK/out" ] && echo yes || echo no)"
printf 'Verified.\n' > "$WORK/section"
rc=0; "$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" >/dev/null 2>&1 || rc=$?
check "a clean section still merges" "0" "$rc"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
