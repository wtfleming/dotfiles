#!/usr/bin/env bash
# Exercise pr-wait.sh against a stand-in gh.
#
# Its failure modes are all a wrong ending: "settled" on a commit that is not the one
# asked about, or on a head whose checks have not all registered, and a long wait thrown
# away by one failed poll.

set -Eeuo pipefail

WAIT="$(cd "$(dirname "$0")/.." && pwd)/scripts/pr-wait.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export TMPDIR="$WORK/tmp"; mkdir "$TMPDIR"
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

# A gh that answers its Nth call with the Nth line of $GH_SAYS, repeating the last one. A
# line starting FAIL fails with the rest of it on stderr.
mkdir "$WORK/bin"
cat > "$WORK/bin/gh" <<'EOF'
#!/usr/bin/env bash
n=$(( $(cat "$GH_COUNT" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$GH_COUNT"
line=$(sed -n "${n}p" "$GH_SAYS"); [ -n "$line" ] || line=$(tail -1 "$GH_SAYS")
case $line in FAIL*) echo "${line#FAIL }" >&2; exit 1 ;; *) echo "$line" ;; esac
EOF
chmod +x "$WORK/bin/gh"
export PATH="$WORK/bin:$PATH" GH_SAYS="$WORK/says" GH_COUNT="$WORK/count"

done_run='{"headRefOid":"abc","statusCheckRollup":[{"__typename":"CheckRun","status":"COMPLETED"},{"__typename":"StatusContext","state":"SUCCESS"}]}'
running='{"headRefOid":"abc","statusCheckRollup":[{"__typename":"CheckRun","status":"IN_PROGRESS"}]}'
status_pending='{"headRefOid":"abc","statusCheckRollup":[{"__typename":"CheckRun","status":"COMPLETED"},{"__typename":"StatusContext","state":"PENDING"}]}'
status_expected='{"headRefOid":"abc","statusCheckRollup":[{"__typename":"StatusContext","state":"EXPECTED"}]}'
no_checks='{"headRefOid":"abc","statusCheckRollup":[]}'
other_head='{"headRefOid":"zzz","statusCheckRollup":[{"__typename":"CheckRun","status":"COMPLETED"}]}'

# Run the wait for commit abc against the given answers; print "<exit>:<polls>:<stdout>".
run() {
  local rc=0 out
  printf '%s\n' "$@" > "$GH_SAYS"; rm -f "$GH_COUNT"
  out="$(PR_WAIT_POLL="${PR_WAIT_POLL:-0}" "$WAIT" 84 abc)" || rc=$?
  echo "$rc:$(cat "$GH_COUNT"):$out"
}

check "finished checks on the named commit settle, on the second poll running" \
  "0:3:settled" "$(run "$running" "$done_run")"
check "one settled poll is not enough when the next shows a check still running" \
  "0:4:settled" "$(run "$done_run" "$running" "$done_run")"
check "a pending commit status counts as unfinished" \
  "0:3:settled" "$(run "$status_pending" "$done_run")"
check "a commit status that is only expected counts as unfinished" \
  "0:3:settled" "$(run "$status_expected" "$done_run")"
check "finished checks on another commit never settle: the head moved" \
  "4:1:head is not abc" "$(PR_WAIT_GRACE=0 run "$other_head")"
check "a stale head is waited out while it is within the grace period" \
  "0:3:settled" "$(run "$other_head" "$done_run")"
check "no checks at all is its own ending, not a pass" \
  "2:1:no checks after 0s" "$(PR_WAIT_GRACE=0 run "$no_checks")"
check "one failed poll in the middle of a wait is ridden out" \
  "0:4:settled" "$(run "$running" "FAIL HTTP 502" "$done_run")"
# Real seconds, because the property is about time: the grace period runs from the first
# failure of a streak, and a good poll in between ends the streak. Two failures three
# seconds apart, under a two-second grace, must not add up.
check "two failed polls with good ones between do not add up to a failing gh" \
  "0:6:settled" "$(PR_WAIT_POLL=1 PR_WAIT_GRACE=2 run "FAIL x" "$running" "$running" "FAIL y" "$done_run")"
check "gh failing for a whole grace period ends the wait with gh's own message" \
  "3:1:gh failing: HTTP 401: Bad credentials" "$(PR_WAIT_GRACE=0 run "FAIL HTTP 401: Bad credentials")"
check "the deadline reports what the last poll saw" \
  "1:1:timed out (last poll: pending)" "$(PR_WAIT_DEADLINE=0 run "$running")"
check "nothing is left in the temp directory" "0" "$(find "$TMPDIR" -type f | wc -l | tr -d ' ')"

rc=0; "$WAIT" 84 >/dev/null 2>&1 || rc=$?
check "a missing argument is a usage error, not a wait" "64" "$rc"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
