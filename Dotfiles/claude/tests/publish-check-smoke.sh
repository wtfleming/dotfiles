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
# An empty list to start with: a list named here and missing is an error.
export PUBLISH_DENYLIST="$WORK/denylist"; : > "$PUBLISH_DENYLIST"
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
check "a Slack token is refused" "1:text:1: Slack token" "$(run "xox""b-${A20:0:12}")"
check "a Google API key is refused" "1:text:1: Google API key" "$(run "AIza""${A20}${A20:0:15}")"
check "an sk- key is refused" "1:text:1: API key (sk-)" "$(run "key=sk""-ant-api03-${A20}")"
check "an sk- placeholder passes" "0:" "$(run 'export KEY=sk-your-api-key-goes-here-xxxx')"
check "a file name that starts sk- passes" "0:" "$(run 'see tests/sk-learn-compatibility-check.py')"
printf 'one\nid=%s\0\nthree\n' "$key" > "$WORK/text"
check "a hit beside a NUL byte still names its line" "1:text:2: AWS access key" \
  "$(rc=0; out=$("$CHECK" "$WORK/text" 2>/dev/null) || rc=$?; echo "$rc:${out#"$WORK"/}")"
check "a cookie value is refused" "1:text:1: Cookie header value" "$(run "Cookie: session=${A20:0:12}")"
check "a header quoted as JSON is refused" "1:text:1: Authorization header value" "$(run "{\"Authorization\": \"Bearer ${A20}\"}")"
check "a lowercase header is refused" "1:text:1: Authorization header value" "$(run "authorization: bearer ${A20}")"
# BSD grep in a UTF-8 locale matches nothing on a line with an invalid byte.
check "a credential beside an invalid UTF-8 byte is refused" "1:text:1: AWS access key" "$(run "$(printf 'caf\351') id=$key")"
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
printf 'ExampleCorp \r\n   internal.example\t\n' > "$PUBLISH_DENYLIST"
check "a term with trailing space and a CRLF ending still matches" "1:text:1: a term on the denylist" "$(run 'deployed at examplecorp.')"
check "an indented term still matches" "1:text:1: a term on the denylist" "$(run 'see internal.example')"
printf '\357\273\277ExampleCorp\n' > "$PUBLISH_DENYLIST"
check "a first term behind a byte-order mark still matches" "1:text:1: a term on the denylist" "$(run 'deployed at examplecorp.')"
printf '# only a comment\n\n' > "$PUBLISH_DENYLIST"
check "a list with no terms checks patterns only" "0:" "$(run 'deployed at examplecorp today')"
check "and says the list held nothing" "1" \
  "$( ("$CHECK" "$WORK/text" 2>&1 >/dev/null || true) | grep -c 'holds no terms')"
rm "$PUBLISH_DENYLIST"
check "a denylist named in the environment and missing fails closed" "2:" "$(run 'anything')"
check "no denylist at the default path is not an error" "0:" \
  "$(unset PUBLISH_DENYLIST; HOME="$WORK/home" run 'deployed at examplecorp today')"
printf 'clean\n' > "$WORK/text"
check "and it says the list was not applied" "1" \
  "$( (unset PUBLISH_DENYLIST; HOME="$WORK/home" "$CHECK" "$WORK/text" 2>&1 >/dev/null || true) | grep -c 'fixed patterns only')"
mkdir -p "$WORK/home/.claude"
ln -s "$WORK/gone" "$WORK/home/.claude/publish-denylist"
check "a default denylist that is a link to nothing fails closed" "2:" \
  "$(unset PUBLISH_DENYLIST; HOME="$WORK/home" run 'anything')"
rm "$WORK/home/.claude/publish-denylist"
mkdir "$PUBLISH_DENYLIST"
check "a denylist that cannot be read fails closed" "2:" "$(run 'anything')"
rmdir "$PUBLISH_DENYLIST"
: > "$PUBLISH_DENYLIST"

echo "case: inputs"
rc=0; "$CHECK" "$WORK/missing" >/dev/null 2>&1 || rc=$?
check "a file that cannot be read is not a clean scan" "2" "$rc"
rc=0; printf '%s\n' "$key" | "$CHECK" - >/dev/null 2>&1 || rc=$?
check "stdin is scanned" "1" "$rc"
rc=0; "$CHECK" >/dev/null 2>&1 || rc=$?
check "no argument is a usage error" "64" "$rc"

echo "case: --tokens-only, for a diff"
# Exit status, then stdout, of a --tokens-only check over one file holding $1.
run_tokens() {
  local rc=0 out
  printf '%s\n' "$1" > "$WORK/text"
  out=$("$CHECK" --tokens-only "$WORK/text" 2>/dev/null) || rc=$?
  echo "$rc:${out#"$WORK"/}"
}
# The sk- shape is matched by its unbroken run, whatever segments a vendor puts first.
for prefix in "" ant-api03- ant-oat01- ant-admin01- ant-sid01- proj- svcacct- admin- or-v1- some-new-vendor-9-; do
  check "sk-${prefix}… is refused in a diff" "1:text:1: API key (sk-)" "$(run_tokens "+key = \"sk""-${prefix}${A20}${A20}\"")"
done
check "a key whose tail has hyphens is refused when one run reaches 20" "1:text:1: API key (sk-)" \
  "$(run_tokens "+k=sk""-ant-api03-${A20:0:6}-${A20}-${A20:0:6}")"
check "an empty segment before the run does not hide a key" "1:text:1: API key (sk-)" \
  "$(run_tokens "+k=sk""-ant-api03-abc--${A20}${A20}")"
check "nor does a hyphen straight after the prefix" "1:text:1: API key (sk-)" "$(run_tokens "+k=sk""---${A20}")"
check "a run of 20 is a key" "1:text:1: API key (sk-)" "$(run_tokens "+k=sk""-x-${A20}")"
check "a run of 19 is not" "0:" "$(run_tokens "+k=sk""-x-${A20:0:19}")"
check "a CSS class that starts sk- passes in a diff" "0:" "$(run_tokens '+  <div class="sk-folding-cube-wrapper-large">')"
check "a CSS custom property passes in a diff" "0:" "$(run_tokens '+  --sk-estimator-doc-link-color: blue;')"
check "a placeholder key passes in a diff" "0:" "$(run_tokens '+export OPENAI_API_KEY=sk-your-api-key-goes-here-xxxx')"
printf '+Authorization: Bearer %s\n+db = "postgres://app:%s@db/app"\n+examplecorp\n' "$A20" "${A20:0:8}" > "$WORK/diff"
printf 'ExampleCorp\n' > "$PUBLISH_DENYLIST"
rc=0; "$CHECK" --tokens-only "$WORK/diff" >/dev/null 2>&1 || rc=$?
check "header, URL and denylist shapes are not checked" "0" "$rc"
printf '+%s\n' "$token" >> "$WORK/diff"
check "a token still is" "1:diff:4: GitHub token" \
  "$(rc=0; out=$("$CHECK" --tokens-only "$WORK/diff" 2>/dev/null) || rc=$?; echo "$rc:${out#"$WORK"/}")"
rm "$PUBLISH_DENYLIST"

echo "case: publish-verify-section.sh refuses a section that fails"
printf 'ExampleCorp\n' > "$PUBLISH_DENYLIST"
printf 'A body.\n' > "$WORK/body"
printf 'Verified.\n%s\n' "$token" > "$WORK/section"
rc=0; "$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" >/dev/null 2>&1 || rc=$?
check "the merge exits 3, the status with no comment fallback" "3" "$rc"
check "nothing was written" "no" "$([ -e "$WORK/out" ] && echo yes || echo no)"
rm "$PUBLISH_DENYLIST"; mkdir "$PUBLISH_DENYLIST"
rc=0; err=$("$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" 2>&1 >/dev/null) || rc=$?
check "a check that could not run refuses the merge the same way" "3" "$rc"
check "and does not tell the reader to redact" "0" "$(printf '%s' "$err" | grep -c 'redact' || true)"
rmdir "$PUBLISH_DENYLIST"
printf 'ExampleCorp\n' > "$PUBLISH_DENYLIST"
check "the refusal for a hit does say to redact" "1" \
  "$( ("$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" 2>&1 >/dev/null || true) | grep -c 'redact')"
printf 'Verified.\n' > "$WORK/section"
rc=0; "$SCRIPTS/publish-verify-section.sh" merge "$WORK/body" "$WORK/section" "$WORK/out" >/dev/null 2>&1 || rc=$?
check "a clean section still merges" "0" "$rc"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
