#!/usr/bin/env bash
# Exercise publish-verify-section.sh against scratch PR bodies.
#
# This script rewrites a public PR description, so its failure mode is deleting prose
# the author wrote, in a place that notifies watchers and cannot be taken back. While
# it lived as a fenced code block in a markdown reference, nothing could reach it:
# the linter only sees *.sh, and neither other smoke test touches it.
#
# Three cases motivated extracting it. `quoted markers` -- a body that quotes this
# mechanism inside a fence, whose lines are not ours to delete. `mid-body` -- a section
# must be replaced where it sits rather than reappearing at the end. And a section that
# itself contains a fence, which is the canonical shape and the one this suite missed on
# its first pass: every fixture was a single line, so a leak that moved a replaced
# section's backticks out into the author's prose passed as green.

set -Eeuo pipefail

PUBLISH="$(cd "$(dirname "$0")/.." && pwd)/scripts/publish-verify-section.sh"
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

check_file() {
  local label=$1 expected=$2 actual=$3
  if diff -q "$expected" "$actual" >/dev/null; then
    echo "  ok    $label"
  else
    echo "  FAIL  $label: files differ" >&2
    diff "$expected" "$actual" >&2 || true
    failures=$((failures + 1))
  fi
}

# Exits 0 on a merge that lands, non-zero on one that refuses. `set -e` is off for the
# call so a refusal is an assertion rather than the end of the suite.
merge_status() {
  local body=$1 section=$2 out=$3 rc=0
  "$PUBLISH" merge "$body" "$section" "$out" >/dev/null 2>&1 || rc=$?
  echo "$rc"
}

printf 'New verdict: verified.\n' > "$WORK/section.md"
printf 'Second verdict: falsified.\n' > "$WORK/section2.md"
: > "$WORK/empty.md"

echo "case: no markers -- section is appended, body survives"
printf '## Summary\n\nRework the thing.\n' > "$WORK/plain.body"
check "merge lands" 0 "$(merge_status "$WORK/plain.body" "$WORK/section.md" "$WORK/plain.out")"
check "section present" 1 "$(grep -c 'New verdict: verified.' "$WORK/plain.out")"
check "body line survives" 1 "$(grep -c 'Rework the thing.' "$WORK/plain.out")"

echo "case: mid-body section is replaced where it sits, not moved to the end"
cat > "$WORK/mid.body" <<'EOF'
## Summary

Rework the thing.

<!-- verify:start -->
Old verdict.
<!-- verify:end -->

## Checklist

- [ ] docs updated
EOF
check "merge lands" 0 "$(merge_status "$WORK/mid.body" "$WORK/section.md" "$WORK/mid.out")"
check "old verdict gone" 0 "$(grep -c 'Old verdict.' "$WORK/mid.out" || true)"
check "new verdict present" 1 "$(grep -c 'New verdict: verified.' "$WORK/mid.out")"
check "checklist survives" 1 "$(grep -c 'docs updated' "$WORK/mid.out")"
# The relocation bug: the section reappeared at EOF, below the checklist.
check "section still above the checklist" ok \
  "$(awk '/New verdict: verified./ {v=NR} /docs updated/ {c=NR} END {print (v && c && v < c) ? "ok" : "moved"}' "$WORK/mid.out")"

echo "case: a rerun is idempotent"
check "second merge lands" 0 "$(merge_status "$WORK/mid.out" "$WORK/section.md" "$WORK/mid.out2")"
check_file "output is stable" "$WORK/mid.out" "$WORK/mid.out2"

echo "case: markers quoted in a fence are the author's text, not a live section"
cat > "$WORK/quoted.body" <<'EOF'
## Summary

Here is what the tool writes:

```markdown
<!-- verify:start -->
## Verification
**Verdict.** Verified.
<!-- verify:end -->
```

That is the shape.
EOF
check "merge lands" 0 "$(merge_status "$WORK/quoted.body" "$WORK/section.md" "$WORK/quoted.out")"
# Two now: the one the author quoted, plus the live section just appended.
check "quoted start marker survives alongside the new one" 2 "$(grep -c 'verify:start' "$WORK/quoted.out")"
check "the quoted marker is still inside the fence" ok \
  "$(awk '/^[ \t]*```/ {fence = !fence} /verify:start/ && fence {q = 1} END {print q ? "ok" : "escaped"}' "$WORK/quoted.out")"
check "quoted verdict line survives" 1 "$(grep -c '\*\*Verdict.\*\* Verified.' "$WORK/quoted.out")"
check "trailing prose survives" 1 "$(grep -c 'That is the shape.' "$WORK/quoted.out")"
check "new section appended" 1 "$(grep -c 'New verdict: verified.' "$WORK/quoted.out")"

echo "case: a section containing a fenced block -- the canonical shape"
# The shape that matters is a fence *inside* the section: references/evidence.md's template
# wraps its raw output in one, inside <details>, so this is what a real run merges. Every
# other fixture here is a single line, which is why the fence leak below shipped once.
# Fixture built rather than sliced out of evidence.md: the property under test is "the
# section contains a fence", and a line range into another file goes stale silently.
cat > "$WORK/fenced.md" <<'EOF'
## Verification

**Verdict.** Verified.

<details><summary>Raw output</summary>

```
200 OK
```

</details>
EOF
check "merge lands" 0 "$(merge_status "$WORK/plain.body" "$WORK/fenced.md" "$WORK/fenced.out")"
check "body line survives" 1 "$(grep -c 'Rework the thing.' "$WORK/fenced.out")"
check "exactly one live section" 1 "$(grep -c 'verify:start' "$WORK/fenced.out")"
check "the section's fence is inside the markers" ok \
  "$(awk '/verify:start/ {s=NR} /^[ \t]*```/ && !f {f=NR} /verify:end/ {e=NR} END {print (s && f && e && s < f && f < e) ? "ok" : "escaped"}' "$WORK/fenced.out")"

echo "case: replacing a live fenced section does not move its fences out"
cat > "$WORK/livefence.body" <<'EOF'
## Summary

keep me

<!-- verify:start -->
## Verification

```
secret excerpt
```
<!-- verify:end -->

## Checklist

- [ ] docs updated
EOF
check "merge lands" 0 "$(merge_status "$WORK/livefence.body" "$WORK/section.md" "$WORK/livefence.out")"
check "the old section's content is gone" 0 "$(grep -c 'secret excerpt' "$WORK/livefence.out" || true)"
# The leak: the replaced section's backtick lines reappeared below verify:end, above the
# author's heading, as a stray empty fence in a public PR body.
check "no fence survives outside the markers" 0 \
  "$("$PUBLISH" strip "$WORK/livefence.out" | grep -c '^[[:space:]]*```' || true)"
check "the author's heading survives" 1 "$(grep -c '## Checklist' "$WORK/livefence.out")"
check "strip removes a live fenced section whole" 0 \
  "$("$PUBLISH" strip "$WORK/livefence.body" | grep -c 'secret excerpt' || true)"

echo "case: refuses a body it cannot act on"
printf 'a\n<!-- verify:start -->\nx\n<!-- verify:end -->\nb\n<!-- verify:start -->\ny\n<!-- verify:end -->\nc\n' > "$WORK/doubled.body"
check "doubled pairs refused" 1 "$(merge_status "$WORK/doubled.body" "$WORK/section.md" "$WORK/doubled.out")"
printf 'a\n<!-- verify:end -->\nx\n<!-- verify:start -->\nb\n' > "$WORK/reversed.body"
check "end before start refused" 1 "$(merge_status "$WORK/reversed.body" "$WORK/section.md" "$WORK/reversed.out")"
printf 'a\n<!-- verify:start -->\nx\n' > "$WORK/unterminated.body"
check "unterminated start refused" 1 "$(merge_status "$WORK/unterminated.body" "$WORK/section.md" "$WORK/unterminated.out")"
check "empty section refused" 1 "$(merge_status "$WORK/plain.body" "$WORK/empty.md" "$WORK/empty.out")"
check "missing body refused" 1 "$(merge_status "$WORK/nope.body" "$WORK/section.md" "$WORK/nope.out")"

echo "case: an empty body is not a licence to publish only the section"
: > "$WORK/blank.body"
check "merge lands" 0 "$(merge_status "$WORK/blank.body" "$WORK/section.md" "$WORK/blank.out")"
check "section present" 1 "$(grep -c 'New verdict: verified.' "$WORK/blank.out")"

echo "case: strip is the gate's invariant"
check "strip drops the live section" 0 "$("$PUBLISH" strip "$WORK/mid.out" | grep -c 'New verdict' || true)"
check "strip keeps a quoted one" 1 "$("$PUBLISH" strip "$WORK/quoted.body" | grep -c 'verify:start')"

echo "case: a replaced section does not disturb the rest"
check "merge lands" 0 "$(merge_status "$WORK/mid.out" "$WORK/section2.md" "$WORK/mid.out3")"
"$PUBLISH" strip "$WORK/mid.out" > "$WORK/before.strip"
"$PUBLISH" strip "$WORK/mid.out3" > "$WORK/after.strip"
check_file "everything outside the markers is byte-identical" "$WORK/before.strip" "$WORK/after.strip"

if [ "$failures" -ne 0 ]; then
  echo "$failures check(s) failed" >&2
  exit 1
fi
echo "all checks passed"
