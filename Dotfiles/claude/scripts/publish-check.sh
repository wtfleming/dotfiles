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

DENYLIST=${PUBLISH_DENYLIST:-$HOME/.claude/publish-denylist}

usage() {
	cat >&2 <<'EOF'
usage: publish-check.sh <file>...
       publish-check.sh -          (read the text from stdin)

Exits 0 when nothing matched, 1 when something did, 2 when a file or the denylist could
not be read. A hit is cleared by redacting the text, as `<redacted: KEY_NAME>`.

The denylist is one term per line, matched as a case-insensitive substring; blank lines
and lines starting with # are skipped. It lives outside the repo, at
~/.claude/publish-denylist or wherever $PUBLISH_DENYLIST points, because the terms it
holds — an employer's name, an internal hostname, a work email domain — are the ones
that must not be published. A machine with no denylist checks the fixed patterns only.
EOF
	exit 64
}

# name<TAB>extended regex. Fixed markers, not entropy: a literal match is a secret.
patterns() {
	cat <<'EOF'
AWS access key	(AKIA|ASIA)[0-9A-Z]{16}
GitHub token	(gh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{22,})
Slack token	xox[abpors]-[0-9A-Za-z-]{10,}
Google API key	AIza[0-9A-Za-z_-]{35}
API key (sk-)	(^|[^A-Za-z0-9])sk-(ant-)?[A-Za-z0-9_-]{20,}
JSON web token	eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}
private key block	-----BEGIN [A-Z ]*PRIVATE KEY-----
Authorization header value	[Aa]uthorization:[[:space:]]*(Bearer|Basic|Token)[[:space:]]+[A-Za-z0-9._~+/=-]{8,}
Cookie header value	[Cc]ookie:[[:space:]]*[A-Za-z0-9_.-]+=[^[:space:];<]{8,}
password in a URL	[a-z][a-z0-9+.-]*://[^/[:space:]:@<]+:[^/[:space:]@<]{3,}@
EOF
}

found=0

# Prints one "file:line: what" per matching line. A grep that fails is not a clean scan:
# status 1 is "no match" and anything higher means the file was never searched.
report() {
	local label=$1 what=$2 rc=0 out
	shift 2
	out=$(grep -n "$@") || rc=$?
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
	local file=$1 label=$2 name re
	if [ ! -f "$file" ] || [ ! -r "$file" ]; then
		echo "publish-check: cannot read $label" >&2
		exit 2
	fi
	while IFS=$'\t' read -r name re; do
		report "$label" "$name" -E -e "$re" -- "$file"
	done < <(patterns)
	[ -z "$terms" ] || report "$label" "a term on the denylist" -i -F -f "$terms" -- "$file"
}

[ $# -ge 1 ] || usage

work=$(mktemp -d) || { echo "publish-check: could not create a temp directory" >&2; exit 2; }
trap 'rm -rf "$work"' EXIT

# An empty pattern line matches every line of the input, so blanks and comments come out
# before grep sees the list.
terms=
if [ -e "$DENYLIST" ]; then
	if [ ! -f "$DENYLIST" ] || [ ! -r "$DENYLIST" ]; then
		echo "publish-check: cannot read the denylist at $DENYLIST" >&2
		exit 2
	fi
	rc=0
	grep -vE '^[[:space:]]*(#|$)' -- "$DENYLIST" > "$work/terms" || rc=$?
	[ "$rc" -le 1 ] || { echo "publish-check: could not parse the denylist" >&2; exit 2; }
	[ ! -s "$work/terms" ] || terms=$work/terms
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
