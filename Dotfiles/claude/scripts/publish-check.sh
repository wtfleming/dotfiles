#!/usr/bin/env bash
# Refuse text that is about to be published when it holds a credential or a term from the
# local denylist. The mechanical half of the scrub in ~/.claude/reference/github-publishing.md.
#
# Prints file:line and what matched, never the matched text: its output lands in a
# transcript, and repeating the secret there is a second leak.
#
# The pattern set and the denylist scan are adapted from Claude-BugHunter's verifier.py and
# scan_identifiers.py (MIT, see ~/.claude/reference/LICENSE-bughunter).
set -euo pipefail

# Bytes, not characters. In a UTF-8 locale BSD grep matches nothing on a line holding one
# invalid byte, so a credential beside a Latin-1 character passed as a clean scan. Every
# pattern here is ASCII. The cost is that a denylist term folds case for ASCII letters only.
export LC_ALL=C

DENYLIST=${PUBLISH_DENYLIST:-$HOME/.claude/publish-denylist}

usage() {
	cat >&2 <<'EOF'
usage: publish-check.sh [--tokens-only] <file>...
       publish-check.sh [--tokens-only] -     (read the text from stdin)

Exits 0 when nothing matched, 1 when something did, 2 when a file or the denylist could
not be read. A hit is cleared by redacting the text, as `<redacted: KEY_NAME>`.

The denylist is one term per line, matched as a case-insensitive substring; blank lines
and lines starting with # are skipped. It lives outside the repo, at
~/.claude/publish-denylist or wherever $PUBLISH_DENYLIST points, because the terms it
holds — an employer's name, an internal hostname, a work email domain — are the ones
that must not be published. A machine with no denylist checks the fixed patterns only,
and says so on stderr. A denylist named by $PUBLISH_DENYLIST that is not there is an
error, since a mistyped path would otherwise read as a clean scan.

--tokens-only checks the vendor token shapes alone: no header or URL shapes and no
denylist. It is for a diff, where those would match ordinary code.
EOF
	exit 64
}

# name<TAB>kind<TAB>extended regex. Fixed markers, not entropy: a literal match is a secret.
# A `token` is a vendor shape with no innocent reading, and is all --tokens-only checks.
# sk- is the one prefix with innocent readings (CSS classes, placeholder text), and vendors
# keep adding segments after it, so it is matched by shape, not by a list of prefixes: sk-,
# then anything, ending in an unbroken run of 20. A key has such a run and a hyphenated
# name does not. The cost is a key with a hyphen in every 20 characters, about 1 in 10,000.
# A `header` is matched without regard to case, raw or as a JSON key, quoted or escaped;
# In a Cookie header any pair counts, since the session rarely comes first. In Set-Cookie
# only the first does: the rest are attributes, and Domain= or Path= is not a secret.
# The first Cookie rule's opening group spells out "not after `set-`", which grep has no
# shorter way to say.
# This is the one list: /wtf-create-pr's pre-flight scans a branch's diff with it too.
patterns() {
	cat <<'EOF'
AWS access key	token	(AKIA|ASIA)[0-9A-Z]{16}
GitHub token	token	(gh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{22,})
Slack token	token	xox[abpors]-[0-9A-Za-z-]{10,}
Google API key	token	AIza[0-9A-Za-z_-]{35}
API key (sk-)	token	(^|[^A-Za-z0-9])sk-[A-Za-z0-9_-]*[A-Za-z0-9_]{20,}
JSON web token	token	eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}
private key block	token	-----BEGIN [A-Z ]*PRIVATE KEY-----
Authorization header value	header	authorization\\?["']?[[:space:]]*:[[:space:]]*\\?["']?(bearer|basic|token)[[:space:]]+[A-Za-z0-9._~+/=-]{8,}
Cookie header value	header	(^|[^-]|(^|[^Tt])-|(^|[^Ee])t-|(^|[^Ss])et-)cookie\\?["']?[[:space:]]*:[[:space:]]*\\?["']?([^;]*;[[:space:]]*)*[A-Za-z0-9_.-]+=[^[:space:];<"']{8,}
Cookie header value	header	set-cookie\\?["']?[[:space:]]*:[[:space:]]*\\?["']?[A-Za-z0-9_.-]+=[^[:space:];<"']{8,}
password in a URL	text	[A-Za-z][A-Za-z0-9+.-]*://[^/[:space:]:@<]*:[^/[:space:]@<]{3,}@
EOF
}

found=0

# Prints one "file:line: what" per matching line. A grep that fails is not a clean scan:
# status 1 is "no match" and anything higher means the file was never searched.
report() {
	local label=$1 what=$2 rc=0 out
	shift 2
	# -a: a NUL byte would otherwise make grep print "Binary file matches" with no line.
	out=$(grep -a -n "$@") || rc=$?
	if [ "$rc" -gt 1 ]; then
		echo "publish-check: could not search $label for: $what" >&2
		exit 2
	fi
	[ "$rc" -eq 0 ] || return 0
	found=1
	printf '%s\n' "$out" | cut -d: -f1 | while read -r n; do
		echo "$label:$n: $what"
	done
}

check_file() {
	local file=$1 label=$2 name kind re
	if [ ! -f "$file" ] || [ ! -r "$file" ]; then
		echo "publish-check: cannot read $label" >&2
		exit 2
	fi
	while IFS=$'\t' read -r name kind re; do
		case $kind in
			token) report "$label" "$name" -E -e "$re" -- "$file" ;;
			header) [ "$tokens_only" -eq 1 ] || report "$label" "$name" -iE -e "$re" -- "$file" ;;
			*) [ "$tokens_only" -eq 1 ] || report "$label" "$name" -E -e "$re" -- "$file" ;;
		esac
	done < <(patterns)
	[ -z "$terms" ] || report "$label" "a term on the denylist" -i -F -f "$terms" -- "$file"
}

tokens_only=0
if [ "${1:-}" = --tokens-only ]; then
	tokens_only=1
	shift
fi
[ $# -ge 1 ] || usage

work=$(mktemp -d) || { echo "publish-check: could not create a temp directory" >&2; exit 2; }
trap 'rm -rf "$work"' EXIT

# An empty pattern line matches every line of the input, so blanks and comments come out
# before grep sees the list. Each term is trimmed first: grep -F takes a line byte for
# byte, so a trailing space or a CRLF ending would leave a term that matches nothing.
terms=
if [ "$tokens_only" -eq 1 ]; then
	:
elif [ -e "$DENYLIST" ]; then
	if [ ! -f "$DENYLIST" ] || [ ! -r "$DENYLIST" ]; then
		echo "publish-check: cannot read the denylist at $DENYLIST" >&2
		exit 2
	fi
	# A byte-order mark stays glued to the first term otherwise, which then never matches.
	bom=$(printf '\357\273\277')
	tr -d '\r' < "$DENYLIST" | sed -e "1s/^$bom//" -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' > "$work/trimmed" \
		|| { echo "publish-check: could not parse the denylist" >&2; exit 2; }
	rc=0
	grep -vE '^(#|$)' -- "$work/trimmed" > "$work/terms" || rc=$?
	[ "$rc" -le 1 ] || { echo "publish-check: could not parse the denylist" >&2; exit 2; }
	if [ -s "$work/terms" ]; then
		terms=$work/terms
	else
		echo "publish-check: the denylist at $DENYLIST holds no terms — fixed patterns only" >&2
	fi
elif [ -n "${PUBLISH_DENYLIST:-}" ] || [ -L "$DENYLIST" ]; then
	# Asked for by name, or a link to nothing: absent here is a mistake, not a choice.
	echo "publish-check: no denylist at $DENYLIST" >&2
	exit 2
else
	echo "publish-check: no denylist at $DENYLIST — fixed patterns only" >&2
fi

for arg in "$@"; do
	if [ "$arg" = - ]; then
		cat > "$work/stdin"
		check_file "$work/stdin" "stdin"
	else
		check_file "$arg" "$arg"
	fi
done

[ "$found" -eq 0 ] || exit 1
