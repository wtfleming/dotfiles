#!/usr/bin/env bash
# Measure /wtf-code-review's false positives: run it over changes that look like defects
# and are not, beside changes that are, and count what it reports. See README.md.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
CASES="$HERE/cases.tsv"

usage() {
	cat >&2 <<'EOF'
usage: run.sh prepare <case> <dir>     build the case as a branch of a new repo at <dir>
       run.sh score <case> <report>    print the result line for a saved report
       run.sh all [<out-dir>] [-- <arguments for /wtf-code-review>]
                                       review every case with `claude -p`, then score

`all` spends model tokens: one review per case, with every lens the case dispatches.
It runs the commands and agents deployed in ~/.claude, not the copies in this repo.
Flags for the `claude` CLI itself go in $REVIEW_TRAPS_CLAUDE_ARGS.
EOF
	exit 64
}

# Prints "ground<TAB>match" for a case, or fails when cases.tsv has no such row.
lookup() {
	awk -F '\t' -v key="$1" '$1 == key { print $2 "\t" $3; found = 1 } END { exit !found }' "$CASES" \
		|| { echo "no such case: $1" >&2; exit 1; }
}

# A pattern that is empty matches every report, and one grep cannot parse matches none:
# either way the row would be scored, and wrongly.
check_match() {
	local key=$1 match=$2 rc=0
	[ -n "$match" ] || { echo "cases.tsv: $key has no pattern" >&2; exit 1; }
	grep -iE -e "$match" < /dev/null > /dev/null 2>&1 || rc=$?
	[ "$rc" -le 1 ] || { echo "cases.tsv: $key has a pattern grep cannot parse: $match" >&2; exit 1; }
}

cmd_prepare() {
	local key=$1 dir=$2
	lookup "$key" >/dev/null
	[ -d "$HERE/cases/$key" ] || { echo "cases.tsv lists $key but cases/$key is missing" >&2; exit 1; }
	[ ! -e "$dir" ] || { echo "refusing to build into an existing path: $dir" >&2; exit 1; }
	mkdir -p "$dir"
	# Its own identity, so the case does not depend on this machine's git config.
	local git=(git -C "$dir" -c user.name=review-traps -c user.email=review-traps@example.invalid)
	"${git[@]}" init -q -b main
	cp -R "$HERE/base/." "$dir/"
	"${git[@]}" add -A
	"${git[@]}" commit -q -m "Base"
	"${git[@]}" switch -q -c change
	cp -R "$HERE/cases/$key/." "$dir/"
	"${git[@]}" add -A
	"${git[@]}" commit -q -m "Change under review"
}

# A finding counts only where the report asks for action: under Critical or Warning.
# Suggestion, Pre-existing and "Judged not worth acting on" are the report declining to.
acted_on() {
	awk '
		/^## / { keep = ($0 ~ /^## (Critical|Warning)[ \t]*$/) ; next }
		keep
	' "$1"
}

cmd_score() {
	local key=$1 report=$2 ground match row hit=no
	row=$(lookup "$key")
	ground=${row%%$'\t'*}
	match=${row#*$'\t'}
	check_match "$key" "$match"
	# No report is a run that failed, never a clean review.
	if [ ! -s "$report" ] || ! grep -q '^# Code Review' "$report"; then
		printf '%-28s %-7s %s\n' "$key" "$ground" "NO REPORT"
		return 0
	fi
	# Captured first: grep -q leaves at its first match, and under pipefail the SIGPIPE
	# that gives awk on a long report turns a hit into "no match".
	local acted
	acted=$(acted_on "$report")
	if printf '%s\n' "$acted" | grep -iE -e "$match" >/dev/null; then hit=yes; fi
	# The review marks a lens that returned no report `see above` on its Lenses line. With
	# one missing, finding nothing says little: it is neither a trap declined nor a miss.
	if [ "$hit" = no ] && grep -qiE '^Lenses:.*see above' "$report"; then
		printf '%-28s %-7s %s\n' "$key" "$ground" "INCOMPLETE — a lens returned no report"
		return 0
	fi
	case $ground:$hit in
		safe:no) printf '%-28s %-7s %s\n' "$key" "$ground" "ok — nothing raised" ;;
		safe:yes) printf '%-28s %-7s %s\n' "$key" "$ground" "FALSE POSITIVE" ;;
		defect:yes) printf '%-28s %-7s %s\n' "$key" "$ground" "ok — found" ;;
		defect:no) printf '%-28s %-7s %s\n' "$key" "$ground" "MISSED" ;;
		*) echo "cases.tsv: $key has ground '$ground', expected safe or defect" >&2; exit 1 ;;
	esac
}

cmd_all() {
	local out="" review_args=()
	while [ $# -gt 0 ]; do
		case $1 in
			--) shift; review_args=("$@"); break ;;
			*) [ -z "$out" ] || usage; out=$1; shift ;;
		esac
	done
	[ -n "$out" ] || out=$(mktemp -d)
	mkdir -p "$out"
	out=$(cd "$out" && pwd)
	local key repo n=0 hold results="$out/results.txt"
	# Every refusal below comes before the first review is paid for, and before
	# results.txt is touched: an earlier run's summary is not this run's to truncate.
	if [ -e "$results" ] || ls -d "$out"/repo-* >/dev/null 2>&1; then
		echo "$out already holds a run — give each run its own directory" >&2
		exit 1
	fi
	# claude reads CLAUDE.md from every directory above where it runs, so a case repo
	# built inside another checkout is reviewed under that checkout's instructions.
	if git -C "$out" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
		echo "$out is inside a git work tree — use a directory outside any repository" >&2
		exit 1
	fi
	local ground row
	while IFS=$'\t' read -r key ground _; do
		[ -n "$key" ] || continue
		[ -d "$HERE/cases/$key" ] || { echo "cases.tsv lists $key but cases/$key is missing" >&2; exit 1; }
		# Through lookup, not read: read folds an empty column into the next one.
		row=$(lookup "$key")
		check_match "$key" "${row#*$'\t'}"
		case $ground in
			safe | defect) ;;
			*) echo "cases.tsv: $key has ground '$ground', expected safe or defect" >&2; exit 1 ;;
		esac
	done < "$CASES"
	local claude_args=()
	# Word-split on purpose: the variable holds a list of flags.
	# shellcheck disable=SC2206
	claude_args=(${REVIEW_TRAPS_CLAUDE_ARGS:-})
	# The run's parent directory is in front of the reviewer, so the two files that name
	# cases are held elsewhere and moved in when the run ends, however it ends.
	hold=$(mktemp -d)
	# Globals: the trap fires after this function's locals are gone.
	HOLD=$hold RUN_OUT=$out
	trap 'mv "$HOLD"/*.txt "$RUN_OUT"/ 2>/dev/null; rmdir "$HOLD" 2>/dev/null' EXIT
	results="$hold/results.txt"
	: > "$results"
	while IFS=$'\t' read -r key _; do
		[ -n "$key" ] || continue
		n=$((n + 1))
		# Numbered, not named: the case name says where the guard is or what the defect
		# is, and the working directory and its parent are in front of the reviewer. So
		# the reports are numbered too.
		repo="$out/repo-$n"
		cmd_prepare "$key" "$repo"
		echo "reviewing $key ..." >&2
		# stdin from /dev/null: claude would otherwise read the case list this loop is on.
		(cd "$repo" && claude -p "/wtf-code-review ${review_args[*]:-}" ${claude_args[@]+"${claude_args[@]}"}) \
			> "$repo.md" 2> "$repo.err" < /dev/null \
			|| echo "  claude exited non-zero for $key — see $repo.err" >&2
		printf '%s\t%s\n' "repo-$n" "$key" >> "$hold/repos.txt"
		cmd_score "$key" "$repo.md" | tee -a "$results"
	done < "$CASES"
	[ "$n" -gt 0 ] || { echo "cases.tsv has no cases" >&2; exit 1; }
	mv "$hold"/*.txt "$out"/
	rmdir "$hold"
	trap - EXIT
	results="$out/results.txt"
	echo
	echo "false positives: $(grep -c 'FALSE POSITIVE' "$results" || true) of $(grep -c ' safe ' "$results" || true) traps"
	echo "missed:          $(grep -c 'MISSED' "$results" || true) of $(grep -c ' defect ' "$results" || true) controls"
	echo "incomplete:      $(grep -c 'INCOMPLETE' "$results" || true) of $n runs"
	echo "no report:       $(grep -c 'NO REPORT' "$results" || true) of $n runs"
	echo "reports and repos are in $out"
}

case ${1:-} in
	prepare) shift; [ $# -eq 2 ] || usage; cmd_prepare "$@" ;;
	score) shift; [ $# -eq 2 ] || usage; cmd_score "$@" ;;
	all) shift; cmd_all "$@" ;;
	*) usage ;;
esac
