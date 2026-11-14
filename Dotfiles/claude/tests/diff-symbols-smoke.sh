#!/usr/bin/env bash
# Exercise diff-symbols.sh against a scratch repository.
#
# Its failure modes are quiet: a changed line credited to the wrong declaration, a file it
# could not parse dropped instead of listed, and a failure that prints an empty result an
# agent reads as "nothing changed".

set -Eeuo pipefail

SYMS="$(cd "$(dirname "$0")/.." && pwd)/scripts/diff-symbols.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
export TMPDIR="$WORK"
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

# The lines of one output section, without its heading.
section() {
  awk -v want="$1:" '$0 == want { on = 1; next } on && /^$/ { exit } on' <<<"$2"
}

git init -q -b main "$WORK/repo"
cd "$WORK/repo"
git config user.email ci@example.invalid
git config user.name CI
cat > a.ts <<'EOF'
export function keep(x: number) {
  return x + 1;
}
class C {
  m() {
    return 3;
  }
}
function gone() {
  return 0;
}
export const k = 1, j = 2;
console.log("top");
EOF
printf 'def pub():\n    return 1\n\ndef _priv():\n    return 2\n\nclass K:\n    def meth(self):\n        return 3\n' > b.py
printf 'class D { n() { return 1; } }\n' > d.js
printf '# doc\n' > c.md
git add . && git commit -q -m base

cat > a.ts <<'EOF'
export function keep(x: number) {
  return x + 2;
}
class C {
  m() {
    return 4;
  }
}
export const k = 1, j = 3;
console.log("top2");
EOF
sed 's/return 3/return 33/; s/return 2/return 22/' b.py > b.py.new && mv b.py.new b.py
printf 'class D { n() { return 2; } }\n' > d.js
printf '# doc2\n' > c.md
printf 'x\n' > untracked.ts

status_before="$(git status --porcelain)"
index_before="$(git ls-files -s | git hash-object --stdin)"
out="$("$SYMS")"
check "leaves the tree alone" "$status_before" "$(git status --porcelain)"
check "leaves the index alone" "$index_before" "$(git ls-files -s | git hash-object --stdin)"
check "prints the command it ran first" "1" "$(head -1 <<<"$out" | grep -c '^# git .* diff -U0 ')"

tab=$'\t'
check "changed: innermost declaration, every one on a shared line, deduplicated" \
  "function${tab}_priv${tab}b.py:4-5${tab}no
function${tab}keep${tab}a.ts:1-3${tab}yes
method${tab}m${tab}a.ts:5-7${tab}no
method${tab}meth${tab}b.py:8-9${tab}no
method${tab}n${tab}d.js:1-1${tab}no
variable${tab}j${tab}a.ts:9-9${tab}yes
variable${tab}k${tab}a.ts:9-9${tab}yes" \
  "$(section changed "$out")"
check "a top-level line is reported, not dropped" "a.ts:10" "$(section top-level "$out")"
check "a deleted declaration is removed, at its old location" \
  "function${tab}gone${tab}a.ts:9-11${tab}no" "$(section "removed (old-side location)" "$out")"
check "an unsupported file is listed, not dropped" \
  "c.md${tab}unsupported language" "$(section "not analyzed" "$out")"
check "an untracked file is named as outside the diff" \
  "untracked.ts" "$(section "untracked (not in the diff)" "$out")"

git add . && git commit -q -m change
printf 'function broken( {\n' > broken.ts
git add broken.ts && git commit -q -m broken
out="$("$SYMS" HEAD~2..HEAD -- broken.ts a.ts)"
check "a range and paths narrow the diff" "a.ts:10" "$(section top-level "$out")"
check "a file that does not parse is listed with the reason" \
  "broken.ts${tab}parse errors" "$(section "not analyzed" "$out")"
check "a committed range has no untracked section" "0" "$(grep -c '^untracked' <<<"$out" || :)"

out="$("$SYMS" HEAD..HEAD)"
check "an empty diff says so" "1" "$(grep -cx 'no changes in this diff' <<<"$out" || :)"

rc=0; "$SYMS" no-such-ref >/dev/null 2>&1 || rc=$?
check "a bad range fails" "1" "$rc"
rc=0; (cd "$WORK" && "$SYMS") >/dev/null 2>&1 || rc=$?
check "outside a repository fails" "1" "$rc"
rc=0; out="$(PATH=/usr/bin:/bin "$SYMS" 2>&1)" || rc=$?
check "missing ast-grep fails rather than printing an empty result" "1:1" \
  "$rc:$(grep -c 'not on PATH' <<<"$out" || :)"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
