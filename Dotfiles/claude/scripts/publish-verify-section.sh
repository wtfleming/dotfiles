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
		function emit(   r, n) {
			if (sectionfile == "") return
			print startm
			n = 0
			while ((r = (getline line < sectionfile)) > 0) { print line; n++ }
			close(sectionfile)
			# getline returns -1 on a read error, which is not EOF. `test -s` in the
			# caller passes on a file this process cannot open, so without this an
			# unreadable section published as two adjacent markers at exit 0 --
			# destroying the section it replaced, which is the empty verdict that
			# check exists to prevent.
			if (r < 0 || n == 0) { bad = 1; exit 1 }
			print endm
		}

		# The tool writes a marker alone on its line, so that is what counts as one.
		# `index()` matched a substring, which made a marker the author mentioned in
		# ordinary prose read as a live delimiter and deleted everything between two
		# such mentions -- at exit 0, because the gate parses the body the same way
		# and so agreed with the damage.
		function is_marker(line, marker,   s) {
			s = line
			gsub(/^[ \t]+|[ \t\r]+$/, "", s)
			return s == marker
		}
		# The run of ` or ~ opening a line, or "" if the line opens no fence. Written
		# without interval expressions, which mawk -- Debian'"'"'s default awk, and so CI'"'"'s --
		# does not reliably support.
		function fence_run(line,   s, ch, n) {
			s = line
			# GitHub returns CRLF for any body typed in the web UI, and a closing
			# fence of ```\r never equalled the ``` that opened it -- so the fence
			# never closed, every marker below it was invisible, and the merge was
			# refused on the ordinary shape of a real PR body.
			sub(/\r$/, "", s)
			sub(/^[ \t]+/, "", s)
			ch = substr(s, 1, 1)
			if (ch != "`" && ch != "~") return ""
			n = 0
			while (substr(s, n + 1, 1) == ch) n++
			if (n < 3) return ""
			return substr(s, 1, n)
		}
		# A closing fence is the run and nothing else; an opening one may carry an info
		# string (```markdown), which is why only the close is held to this.
		function fence_only(line,   s) {
			s = line
			gsub(/[ \t\r]/, "", s)
			return s == fence_run(line)
		}
		{
			run = fence_run($0)
			if (fence) {
				# Only the same character, at least as long, alone on its line, closes
				# it. A ``` inside a ~~~ block is content -- treating it as a delimiter
				# clears the state and hands the markers below a quoted section to
				# delete, which the gate cannot catch because it parses the same way.
				if (run != "" && substr(run, 1, 1) == fence_char \
				    && length(run) >= fence_len && fence_only($0)) fence = 0
				if (!skip) print
				next
			}
			if (run != "") {
				# A fence *inside* the section being replaced is section content: discard
				# it, and do not track it. Tracking would leave the fence state describing
				# text that is not in the output, which is what the gate compares.
				if (skip) next
				fence = 1
				fence_char = substr(run, 1, 1)
				fence_len = length(run)
				print
				next
			}
		}
		!fence && is_marker($0, startm) {
			if (skip || seen) { bad = 1; exit 1 }
			skip = 1; seen = 1
			emit()
			next
		}
		!fence && is_marker($0, endm) {
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
	# `-s` is a size test and passes on a file this process cannot open, which then
	# surfaced two guards later as a complaint about the *body's* markers -- sending
	# someone hand-fixing a public PR to the one thing that is not wrong.
	[ -r "$section" ] || { echo "cannot read the section file: $section" >&2; exit 1; }
	# A section line that is a marker by is_marker's rule nests a second pair inside
	# the first. The written body then holds two starts and two ends, which this
	# script's own reader refuses on every later run -- so the PR is stuck displaying a
	# verdict no rerun can replace, which is the exact failure the delimiters exist to
	# prevent. The gate cannot catch it, because it strips the section from both sides.
	# A marker mentioned mid-line is prose to the reader, so it is allowed through.
	if grep -qxE -e "[[:space:]]*($START|$END)[[:space:]]*" -- "$section"; then
		echo "the section contains a verify marker; it would nest a second pair — refusing" >&2
		exit 1
	fi

	local tmp before after
	tmp=$(mktemp) || { echo "could not create a temp file" >&2; exit 1; }
	before=$(mktemp) || { echo "could not create a temp file" >&2; exit 1; }
	after=$(mktemp) || { echo "could not create a temp file" >&2; exit 1; }
	# shellcheck disable=SC2064  # expand the paths now, so the trap fires on the real ones
	trap "rm -f '$tmp' '$tmp.nonl' '$before' '$after'" EXIT

	if ! run_awk "$body" "$section" > "$tmp"; then
		echo "verify markers in $body are doubled, out of order or unterminated — fix it by hand" >&2
		exit 1
	fi

	# The gate. Everything outside the markers must survive byte-for-byte and in
	# order: this is what catches a marker the author only quoted (whose lines are
	# not ours to delete) and a section that would move rather than be replaced.
	# Trailing blank lines are normalised away on both sides, because appending a
	# section to a body that had none must add its own separator.
	#
	# Materialised to files with checked statuses, never compared as process
	# substitutions: `<(...)` discards the exit status, so a gate_view that aborted
	# part-way fed `diff` truncated output that happened to match, and the gate
	# passed on an input it exists to refuse.
	gate_view "$body" > "$before" \
		|| { echo "could not parse $body — nothing written" >&2; exit 1; }
	gate_view "$tmp" > "$after" \
		|| { echo "could not parse the merged body — nothing written" >&2; exit 1; }

	# awk's `print` terminates every line it emits, so a body whose final line
	# carried no newline gains one -- a byte changed outside the markers, on the
	# author's own last line. `gate_view` reprints both sides, so the gate agrees
	# with it. Match the source's final-newline state before comparing.
	if [ -s "$body" ] && [ "$(tail -c1 "$body" | wc -l)" -eq 0 ] \
		&& [ -s "$tmp" ] && [ "$(tail -c1 "$tmp" | wc -l)" -ne 0 ]; then
		# Exactly one byte, never `$(cat)`, which would eat every trailing blank
		# line and change the text outside the markers a second way.
		head -c "$(( $(wc -c < "$tmp") - 1 ))" "$tmp" > "$tmp.nonl" \
			|| { echo "could not normalise the final newline" >&2; exit 1; }
		mv "$tmp.nonl" "$tmp"
	fi

	if ! diff -q "$before" "$after" >/dev/null; then
		echo "refusing to write: the merge changed content outside the section" >&2
		diff "$before" "$after" >&2 || true
		exit 1
	fi

	cat "$tmp" > "$out"
}

case ${1:-} in
	merge) shift; [ $# -eq 3 ] || usage; cmd_merge "$@" ;;
	strip) shift; [ $# -eq 1 ] || usage; cmd_strip "$@" ;;
	*) usage ;;
esac
