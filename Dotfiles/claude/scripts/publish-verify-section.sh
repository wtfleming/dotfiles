#!/usr/bin/env bash
# Merge a generated verification section into a PR body, between the
# verify:start / verify:end markers named in ~/.claude/reference/github-publishing.md.
#
# Extracted from that reference so shellcheck and Dotfiles/claude/tests/ can reach it:
# it deletes the author's prose from a public PR when it is wrong, and a fenced code
# block in a markdown file is invisible to both.
set -euo pipefail

START='<!-- verify:start -->'
END='<!-- verify:end -->'

usage() {
	cat >&2 <<'EOF'
usage: publish-verify-section.sh merge <body-file> <section-file> <out-file>
       publish-verify-section.sh strip <body-file>

  merge  Replace the body's verification section with <section-file>, in place;
         append it at the end when the body has none. Refuses on a body whose
         markers are doubled, out of order or unterminated, and on any merge that
         would change a byte outside the section.
  strip  Print the body with its verification section removed. The merge gate is
         `strip` before == `strip` after, which is the ownership rule's claim that
         every byte outside the markers is carried across untouched.

A marker inside a fenced code block is the author quoting this mechanism, not a
section this tool owns, and is left alone.
EOF
	exit 64
}

# Emits the body with the live section either replaced by $2 or removed.
# Exits 1 on a marker layout it will not act on, rather than guessing.
run_awk() {
	local body=$1 sectionfile=$2
	awk -v startm="$START" -v endm="$END" -v sectionfile="$sectionfile" '
		function emit() {
			if (sectionfile == "") return
			print startm
			while ((getline line < sectionfile) > 0) print line
			close(sectionfile)
			print endm
		}
		# ``` or ~~~ toggles fenced state. Markers only count outside a fence.
		# Both the toggle and the print are gated on !skip: a fence *inside* the section
		# being replaced is section content, so printing it would move it outside the
		# markers, and toggling on it would leave the fence state describing text that
		# is being discarded.
		/^[ \t]*(```|~~~)/ { if (!skip) { fence = !fence; print } next }
		!fence && index($0, startm) {
			if (skip || seen) { bad = 1; exit 1 }
			skip = 1; seen = 1
			emit()
			next
		}
		!fence && index($0, endm) {
			if (!skip) { bad = 1; exit 1 }
			skip = 0
			next
		}
		!skip
		END {
			if (bad || skip) exit 1
			if (!seen) {
				if (sectionfile == "") exit 0
				print ""
				emit()
			}
		}
	' "$body"
}

cmd_strip() {
	local body=${1:?}
	[ -f "$body" ] || { echo "no such body file: $body" >&2; exit 1; }
	run_awk "$body" ""
}

# `strip`, with trailing blank lines removed, which is what the gate compares.
gate_view() {
	cmd_strip "$1" | awk '
		{ line[NR] = $0 }
		END {
			last = NR
			while (last > 0 && line[last] ~ /^[ \t]*$/) last--
			for (i = 1; i <= last; i++) print line[i]
		}
	'
}

cmd_merge() {
	local body=${1:?} section=${2:?} out=${3:?}
	[ -f "$body" ] || { echo "no such body file: $body" >&2; exit 1; }
	# An empty section between two markers publishes as a verdict with no content.
	[ -s "$section" ] || { echo "no section to publish" >&2; exit 1; }

	local tmp
	tmp=$(mktemp) || { echo "could not create a temp file" >&2; exit 1; }
	# shellcheck disable=SC2064  # expand $tmp now, so the trap fires on the real path
	trap "rm -f '$tmp'" EXIT

	if ! run_awk "$body" "$section" > "$tmp"; then
		echo "verify markers missing, doubled or out of order — fix it by hand" >&2
		exit 1
	fi

	# The gate. Everything outside the markers must survive byte-for-byte and in
	# order: this is what catches a marker the author only quoted (whose lines are
	# not ours to delete) and a section that would move rather than be replaced.
	# Trailing blank lines are normalised away on both sides, because appending a
	# section to a body that had none must add its own separator.
	if ! diff -q <(gate_view "$body") <(gate_view "$tmp") >/dev/null; then
		echo "refusing to write: the merge changed content outside the section" >&2
		diff <(gate_view "$body") <(gate_view "$tmp") >&2 || true
		exit 1
	fi

	cat "$tmp" > "$out"
}

case ${1:-} in
	merge) shift; cmd_merge "$@" ;;
	strip) shift; cmd_strip "$@" ;;
	*) usage ;;
esac
