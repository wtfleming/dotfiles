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
	# No report is a run that failed, never a clean review.
	if [ ! -s "$report" ] || ! grep -q '^# Code Review' "$report"; then
		printf '%-28s %-7s %s\n' "$key" "$ground" "NO REPORT"
		return 0
	fi
	if acted_on "$report" | grep -qiE -e "$match"; then hit=yes; fi
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
	local key repo n=0 results="$out/results.txt"
	local claude_args=()
	# Word-split on purpose: the variable holds a list of flags.
	# shellcheck disable=SC2206
	claude_args=(${REVIEW_TRAPS_CLAUDE_ARGS:-})
	: > "$results"
	while IFS=$'\t' read -r key _; do
		[ -n "$key" ] || continue
		n=$((n + 1))
		repo="$out/repo-$key"
		cmd_prepare "$key" "$repo"
		echo "reviewing $key ..." >&2
		# stdin from /dev/null: claude would otherwise read the case list this loop is on.
		(cd "$repo" && claude -p "/wtf-code-review ${review_args[*]:-}" ${claude_args[@]+"${claude_args[@]}"}) \
			> "$out/$key.md" 2> "$out/$key.err" < /dev/null \
			|| echo "  claude exited non-zero for $key — see $out/$key.err" >&2
		cmd_score "$key" "$out/$key.md" | tee -a "$results"
	done < "$CASES"
	[ "$n" -gt 0 ] || { echo "cases.tsv has no cases" >&2; exit 1; }
	echo
	echo "false positives: $(grep -c 'FALSE POSITIVE' "$results" || true) of $(grep -c ' safe ' "$results" || true) traps"
	echo "missed:          $(grep -c 'MISSED' "$results" || true) of $(grep -c ' defect ' "$results" || true) controls"
	echo "no report:       $(grep -c 'NO REPORT' "$results" || true) of $n runs"
	echo "reports and repos are in $out"
}

case ${1:-} in
	prepare) shift; [ $# -eq 2 ] || usage; cmd_prepare "$@" ;;
	score) shift; [ $# -eq 2 ] || usage; cmd_score "$@" ;;
	all) shift; cmd_all "$@" ;;
	*) usage ;;
esac
