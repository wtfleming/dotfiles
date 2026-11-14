#!/usr/bin/env bash
# Wait until a pull request's checks have finished on one named commit.
#
# Usage:
#   pr-wait.sh <pr> <sha>
#
# Exit status, each with one line on stdout saying why:
#   0  settled: the PR's head is <sha> and every check on it has finished
#   1  timed out
#   2  no checks ever appeared -- the repo runs none on this PR
#   3  gh failed on every poll for a whole grace period, so the state is unknown
#   4  the PR's head is some other commit
#  64  called wrongly, or gh or jq is missing: nothing was waited for (message on stderr)
#
# Naming the commit is what makes "settled" mean this code: just after a push GitHub still
# serves the previous commit's finished checks, and a wait that asked only "is anything
# pending?" ends on those.
#
# PR_WAIT_POLL, PR_WAIT_GRACE and PR_WAIT_DEADLINE (seconds; 30, 300, 1800) exist so the
# smoke test can run the loop without waiting.

set -Eeuo pipefail

[ $# -eq 2 ] || { echo "usage: pr-wait.sh <pr> <sha>" >&2; exit 64; }
for tool in gh jq; do
  command -v "$tool" >/dev/null 2>&1 || { echo "error: $tool is not on PATH" >&2; exit 64; }
done
pr=$1 sha=$2
poll=${PR_WAIT_POLL:-30} grace=${PR_WAIT_GRACE:-300} deadline=${PR_WAIT_DEADLINE:-1800}

err=$(mktemp); trap 'rm -f "$err"' EXIT   # gh's stderr, kept for the exit-3 message

# One answer per poll, from the status rollup of the PR's head. A check run is finished
# when COMPLETED; a commit status when it is neither PENDING nor EXPECTED.
# shellcheck disable=SC2016 # $sha is a jq variable, passed with --arg
query='if .headRefOid != $sha then "moved"
  elif (.statusCheckRollup | length) == 0 then "none"
  elif any(.statusCheckRollup[]; if .__typename == "CheckRun" then .status != "COMPLETED"
    else (.state == "PENDING" or .state == "EXPECTED") end) then "pending"
  else "settled" end'

start=$(date +%s) quiet=0 err_since=""
while :; do
  s=$(gh pr view "$pr" --json headRefOid,statusCheckRollup 2>"$err" | jq -r --arg sha "$sha" "$query" 2>>"$err") || s=error
  now=$(date +%s); elapsed=$(( now - start ))
  # Twice running: one check can register before the others exist.
  if [ "$s" = settled ]; then
    quiet=$((quiet + 1))
    [ "$quiet" -lt 2 ] || { echo "settled"; exit 0; }
  else
    quiet=0
  fi
  # gh must fail for a whole grace period to count: one bad poll in a long wait is a blip.
  if [ "$s" = error ]; then
    : "${err_since:=$now}"
    [ $(( now - err_since )) -lt "$grace" ] || { echo "gh failing: $(cat "$err")"; exit 3; }
  else
    err_since=""
  fi
  if [ "$elapsed" -ge "$grace" ]; then
    case $s in
      none) echo "no checks after ${grace}s"; exit 2 ;;
      moved) echo "head is not $sha"; exit 4 ;;
    esac
  fi
  [ "$elapsed" -lt "$deadline" ] || { echo "timed out (last poll: $s)"; exit 1; }
  sleep "$poll"
done
