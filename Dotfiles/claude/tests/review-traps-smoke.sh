#!/usr/bin/env bash
# Exercise evals/review-traps/run.sh without spending a review.
#
# The benchmark's output is a number someone will decide on, so its failure modes are a
# case that cannot be built, discovered after earlier reviews were paid for, and a score
# that counts the wrong section of a report. `all` is driven with a stand-in claude.

set -Eeuo pipefail

TRAPS="$(cd "$(dirname "$0")/.." && pwd)/evals/review-traps"
RUN="$TRAPS/run.sh"
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

echo "case: every row of cases.tsv builds a repo with a change in it"
rows=0
while IFS=$'\t' read -r key ground _; do
  [ -n "$key" ] || continue
  rows=$((rows + 1))
  rc=0; "$RUN" prepare "$key" "$WORK/prep-$key" >/dev/null 2>&1 || rc=$?
  check "$key builds" "0" "$rc"
  changed=$(git -C "$WORK/prep-$key" diff --name-only main change 2>/dev/null | wc -l | tr -d ' ')
  check "$key changes something" "yes" "$([ "${changed:-0}" -gt 0 ] && echo yes || echo no)"
  check "$key is safe or defect" "1" "$(printf '%s\n' "$ground" | grep -cxE 'safe|defect' || true)"
done < "$TRAPS/cases.tsv"
check "there are cases" "yes" "$([ "$rows" -gt 0 ] && echo yes || echo no)"
check "every case is in the README's table" "$rows" "$(cut -f1 "$TRAPS/cases.tsv" | while read -r k; do grep -c -F "| \`$k\` |" "$TRAPS/README.md" || true; done | grep -cx 1 || true)"
check "every case directory has a row" "$rows" "$(find "$TRAPS/cases" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
rc=0; "$RUN" prepare sql-value-in-query "$WORK/prep-sql-value-in-query" >/dev/null 2>&1 || rc=$?
check "prepare refuses a path that exists" "1" "$rc"

echo "case: score reads only the sections that ask for action"
result() { "$RUN" score "$1" "$2" | sed 's/^[^ ]* *[^ ]* *//'; }
printf '# Code Review\n\n## Warning\n- SQL injection through the sort column.\n' > "$WORK/hit.md"
printf '# Code Review\n\n## Suggestion\n- possible injection\n\n## Judged not worth acting on\n- **Warning** injection\n' > "$WORK/aside.md"
printf '# Code Review\n\n## Warning\n- no test for the new handler\n' > "$WORK/other.md"
: > "$WORK/empty.md"
printf 'I could not run the review.\n' > "$WORK/prose.md"
check "a match under Warning on a trap is a false positive" "FALSE POSITIVE" "$(result sql-allowlisted-column "$WORK/hit.md")"
check "a match only under Suggestion or set aside is not" "ok — nothing raised" "$(result sql-allowlisted-column "$WORK/aside.md")"
check "an unrelated Warning is not" "ok — nothing raised" "$(result sql-allowlisted-column "$WORK/other.md")"
check "a match on a control is found" "ok — found" "$(result sql-value-in-query "$WORK/hit.md")"
check "no match on a control is a miss" "MISSED" "$(result sql-value-in-query "$WORK/other.md")"
check "an empty report is not a clean one" "NO REPORT" "$(result sql-allowlisted-column "$WORK/empty.md")"
check "output with no report heading is not a clean one" "NO REPORT" "$(result sql-allowlisted-column "$WORK/prose.md")"
# A match on the first line of a long section: a scorer that stops reading at the match
# kills the writer mid-report, and under pipefail that reads as no match.
{ printf '# Code Review\n\n## Warning\n- SQL injection through the sort column.\n'
  i=0; while [ "$i" -lt 4000 ]; do printf -- '- another finding that pads the section out, number %s\n' "$i"; i=$((i + 1)); done
} > "$WORK/long.md"
check "a match at the top of a long report is still found" "ok — found" "$(result sql-value-in-query "$WORK/long.md")"
printf '# Code Review\n\n## Warning\n- no test\n\nLenses: security — see above · tests — 1 finding\n' > "$WORK/partial.md"
check "nothing raised with a lens missing is incomplete, not declined" "INCOMPLETE — a lens returned no report" "$(result sql-allowlisted-column "$WORK/partial.md")"
check "nor is it a miss" "INCOMPLETE — a lens returned no report" "$(result sql-value-in-query "$WORK/partial.md")"
printf '# Code Review\n\n## Warning\n- SQL injection.\n\nLenses: security — see above\n' > "$WORK/partial-hit.md"
check "a match still counts with a lens missing" "FALSE POSITIVE" "$(result sql-allowlisted-column "$WORK/partial-hit.md")"
rc=0; "$RUN" score no-such-case "$WORK/hit.md" >/dev/null 2>&1 || rc=$?
check "an unknown case is an error" "1" "$rc"

echo "case: all, with a stand-in claude that reports injection everywhere"
mkdir "$WORK/bin"
cat > "$WORK/bin/claude" <<'EOF'
#!/usr/bin/env bash
# Drain stdin as the real one does: a caller that leaves its case list on stdin loses it.
cat > /dev/null
printf '%s\n' "$PWD" >> "$CLAUDE_CALLS"
# What a reviewer who looks one directory up can read while it works.
{ ls ..; cat ../*.txt; } >> "$CLAUDE_SEEN" 2>/dev/null
printf '%s\n' "$*" >> "$CLAUDE_ARGS"
printf '# Code Review\n\n## Critical\n- SQL injection.\n'
EOF
chmod +x "$WORK/bin/claude"
export CLAUDE_CALLS="$WORK/calls" CLAUDE_ARGS="$WORK/args" CLAUDE_SEEN="$WORK/seen"
out=$(PATH="$WORK/bin:$PATH" "$RUN" all "$WORK/run1" -- --lite 2>/dev/null) && rc=0 || rc=$?
check "it finishes" "0" "$rc"
check "it reviewed every case" "$rows" "$(wc -l < "$CLAUDE_CALLS" | tr -d ' ')"
check "each review ran inside its own case repo" "$rows" "$(sort -u "$CLAUDE_CALLS" | grep -c "^$WORK/run1/repo-" || true)"
check "no working directory names its case" "0" "$(cut -f1 "$TRAPS/cases.tsv" | grep -c -F -f - "$CLAUDE_CALLS" || true)"
check "nor does anything beside it, while the review runs" "0" "$(cut -f1 "$TRAPS/cases.tsv" | grep -c -F -f - "$CLAUDE_SEEN" || true)"
check "the mapping from directory to case is kept" "$rows" "$(wc -l < "$WORK/run1/repos.txt" | tr -d ' ')"
check "the review arguments reach claude" "$rows" "$(grep -c -- '/wtf-code-review --lite' "$CLAUDE_ARGS" || true)"
check "flags from the environment reach claude" "1" \
  "$(: > "$CLAUDE_ARGS"; REVIEW_TRAPS_CLAUDE_ARGS='--model x' PATH="$WORK/bin:$PATH" "$RUN" all "$WORK/run-flags" >/dev/null 2>&1; sed -n 1p "$CLAUDE_ARGS" | grep -c -- '--model x$' || true)"
check "the traps that mention injection count as false positives" "false positives: 2 of 4 traps" "$(printf '%s\n' "$out" | grep '^false positives')"
check "the control it names is found, the other missed" "missed:          1 of 2 controls" "$(printf '%s\n' "$out" | grep '^missed')"
cp "$WORK/run1/results.txt" "$WORK/results.before"
rc=0; PATH="$WORK/bin:$PATH" "$RUN" all "$WORK/run1" >/dev/null 2>&1 || rc=$?
check "a second run into the same directory is refused" "1" "$rc"
check "and the first run's results are untouched" "same" "$(cmp -s "$WORK/results.before" "$WORK/run1/results.txt" && echo same || echo changed)"
git init -q "$WORK/tree"
: > "$CLAUDE_CALLS"
rc=0; PATH="$WORK/bin:$PATH" "$RUN" all "$WORK/tree/out" >/dev/null 2>&1 || rc=$?
check "a directory inside a git work tree is refused" "1" "$rc"
check "before any review runs" "0" "$(wc -l < "$CLAUDE_CALLS" | tr -d ' ')"

echo "case: a run that is killed keeps what it had"
mkdir "$WORK/killbin"
cat > "$WORK/killbin/claude" <<'EOF'
#!/usr/bin/env bash
cat > /dev/null
kill -TERM "$(cat "$RUN_PID_FILE")"
EOF
chmod +x "$WORK/killbin/claude"
# An apostrophe in the path: the paths reach the exit trap as variables, not spliced text.
rc=0; RUN_PID_FILE="$WORK/pid" PATH="$WORK/killbin:$PATH" \
  bash -c 'echo $$ > "$RUN_PID_FILE"; exec "$0" all "$1"' "$RUN" "$WORK/q'x" >/dev/null 2>&1 || rc=$?
check "it dies of the signal" "143" "$rc"
check "its results are in the output directory" "yes" "$([ -e "$WORK/q'x/results.txt" ] && echo yes || echo no)"

echo "case: a broken cases.tsv is refused before any review runs"
cp -R "$TRAPS" "$WORK/copy"
printf 'no-such-dir\tsafe\tinject\twhy\n' >> "$WORK/copy/cases.tsv"
: > "$CLAUDE_CALLS"
rc=0; PATH="$WORK/bin:$PATH" "$WORK/copy/run.sh" all "$WORK/run-missing" >/dev/null 2>&1 || rc=$?
check "a row with no case directory is refused" "1" "$rc"
check "with no review run" "0" "$(wc -l < "$CLAUDE_CALLS" | tr -d ' ')"
cp "$TRAPS/cases.tsv" "$WORK/copy/cases.tsv"
mkdir "$WORK/copy/cases/odd"
printf 'odd\tmaybe\tinject\twhy\n' >> "$WORK/copy/cases.tsv"
rc=0; PATH="$WORK/bin:$PATH" "$WORK/copy/run.sh" all "$WORK/run-ground" >/dev/null 2>&1 || rc=$?
check "a row that is neither safe nor defect is refused" "1" "$rc"
check "with no review run" "0" "$(wc -l < "$CLAUDE_CALLS" | tr -d ' ')"

cp "$TRAPS/cases.tsv" "$WORK/copy/cases.tsv"
rmdir "$WORK/copy/cases/odd"
mkdir "$WORK/copy/cases/blank" "$WORK/copy/cases/broken"
printf 'blank\tsafe\t\twhy\n' >> "$WORK/copy/cases.tsv"
rc=0; PATH="$WORK/bin:$PATH" "$WORK/copy/run.sh" all "$WORK/run-blank" >/dev/null 2>&1 || rc=$?
check "a row with an empty pattern is refused" "1" "$rc"
cp "$TRAPS/cases.tsv" "$WORK/copy/cases.tsv"
printf 'broken\tsafe\t(sql\twhy\n' >> "$WORK/copy/cases.tsv"
rc=0; PATH="$WORK/bin:$PATH" "$WORK/copy/run.sh" all "$WORK/run-broken" >/dev/null 2>&1 || rc=$?
check "a row whose pattern grep cannot parse is refused" "1" "$rc"
check "with no review run for either" "0" "$(wc -l < "$CLAUDE_CALLS" | tr -d ' ')"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
