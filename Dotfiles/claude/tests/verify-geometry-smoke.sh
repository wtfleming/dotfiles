#!/usr/bin/env bash
# Exercise wtf-show-me's verify_geometry.py against small SVG pages.
#
# It is the only check on connector geometry a wtf-show-me page gets before a
# person looks at it, so its failure mode is a broken connector passing -- one
# rule not checked, or a path it could not read counted as clean.

set -Eeuo pipefail

CHECK="$(cd "$(dirname "$0")/.." && pwd)/skills/wtf-show-me/scripts/verify_geometry.py"
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

# page NAME CONNECTORS [MASK_X]: two boxes, painted after the connectors and a label mask.
page() {
  cat > "$WORK/$1.html" <<EOF
<svg viewBox="0 0 400 140" role="img">
$2
<rect x="${3:-172}" y="44" width="56" height="12" fill="#f5f5f5"/>
<rect x="40" y="40" width="120" height="48" rx="6" fill="#f5f5f5" stroke="#2d3142" stroke-width="1"/>
<rect x="240" y="40" width="120" height="48" rx="6" fill="#f5f5f5" stroke="#2d3142" stroke-width="1"/>
</svg>
EOF
}

# findings NAME PATTERN: how many findings for that page match the pattern.
findings() {
  python3 -I "$CHECK" "$WORK/$1.html" | grep -c "$2" || :
}

arrow='stroke="#4f5d75" marker-end="url(#arrow)"'

page clean "<line x1=\"160\" y1=\"64\" x2=\"240\" y2=\"64\" $arrow/>"
rc=0; python3 -I "$CHECK" "$WORK/clean.html" >/dev/null || rc=$?
check "a clean page passes" "0" "$rc"

page diagonal "<line x1=\"160\" y1=\"64\" x2=\"240\" y2=\"72\" $arrow/>"
check "a diagonal connector is reported" "1" "$(findings diagonal 'diagonal segment')"

page clipped "<line x1=\"160\" y1=\"64\" x2=\"240\" y2=\"64\" $arrow/>" 230
check "a label mask under a later box is reported" "1" "$(findings clipped 'clipped by node')"

page ports "<line x1=\"160\" y1=\"60\" x2=\"240\" y2=\"60\" $arrow/>
<line x1=\"160\" y1=\"66\" x2=\"240\" y2=\"66\" $arrow/>"
check "ports 6px apart are reported, on both boxes" "2" "$(findings ports 'fan the ports')"

page unreadable "<path d=\"M 160 64 X 240\" fill=\"none\" $arrow/>"
rc=0; python3 -I "$CHECK" "$WORK/unreadable.html" >/dev/null || rc=$?
check "path data it cannot read fails the run" "1" "$rc"

if [ "$failures" -gt 0 ]; then
  echo "$failures check(s) failed" >&2
  exit 1
fi
echo "all checks passed"
