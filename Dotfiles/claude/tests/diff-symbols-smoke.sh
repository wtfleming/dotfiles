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

out="$("$SYMS" HEAD..HEAD)"
check "an empty diff says so" "1" "$(grep -cx 'no changes in this diff' <<<"$out" || :)"
check "A...B diffs from the merge base" "a.ts:10" "$(section top-level "$("$SYMS" HEAD~2...HEAD -- a.ts)")"
printf 'still\n' > still-untracked.ts
out="$("$SYMS" HEAD~2..HEAD)"
check "a committed range has no untracked section, even with untracked files about" \
  "0" "$(grep -cx 'untracked (not in the diff):' <<<"$out" || :)"
check "a single rev diffs against the working tree" \
  "still-untracked.ts" "$(section "untracked (not in the diff)" "$("$SYMS" HEAD~1)")"
rm still-untracked.ts

# Second repository: the cases a line diff gets wrong without care.
git init -q -b main "$WORK/two"
cd "$WORK/two"
git config user.email ci@example.invalid
git config user.name CI
cat > s.ts <<'EOF'
class P {
  q() {
    return 1;
  }
}
function lonely() {
  return 0;
}
function body() {
  const a = 1;
  const b = 2;
  return a + b;
}
EOF
cat > m.py <<'EOF'
class A:
    def __init__(self):
        self.x = 1

class B:
    def __init__(self):
        self.y = 2

    @staticmethod
    def s():
        return 3
EOF
printf 'export function evil() {\n  return 1;\n}\n' > ./--globs=z.ts
printf 'export function decoy() {\n  return 1;\n}\n' > z.ts
printf '\x00\x01' > blob.bin
printf 'export const m = 1;\n' > mode.ts
mkdir sub && printf 'def f():\n    return 1\n' > sub/n.py
git add . && git commit -q -m base

# Delete lonely() alone, and one body line of body(), each its own hunk.
sed -e '/^function lonely/,/^}/d' -e '/const b = 2;/d' s.ts > s.ts.new && mv s.ts.new s.ts
# Delete A.__init__ only; B keeps one of the same name. Change B.s's decorator.
printf 'class A:\n    pass\n\nclass B:\n    def __init__(self):\n        self.y = 2\n\n    @classmethod\n    def s():\n        return 3\n' > m.py
printf 'export function evil() {\n  return 2;\n}\n' > ./--globs=z.ts
printf '\x00\x02' > blob.bin
chmod +x mode.ts
: > empty.ts && git add -N empty.ts
printf 'def f():\n    return 2\n' > sub/n.py

out="$("$SYMS")"
changed="$(section changed "$out")"
check "a body-line deletion credits the function that lost it" "1" \
  "$(grep -c "^function${tab}body${tab}" <<<"$changed" || :)"
check "a whole-declaration deletion is not credited to its neighbour" "0" \
  "$(grep -c "^class${tab}P${tab}" <<<"$changed" || :)"
check "the deleted declaration is removed" "function${tab}lonely${tab}s.ts:6-8${tab}no" \
  "$(section "removed (old-side location)" "$out" | grep lonely || :)"
check "deleting one of two same-named methods is a removal" \
  "method${tab}__init__${tab}m.py:2-3${tab}no" \
  "$(section "removed (old-side location)" "$out" | grep __init__ || :)"
check "a decorator change credits the decorated method" "1" \
  "$(grep -c "^method${tab}s${tab}m.py:8-10${tab}no" <<<"$changed" || :)"
check "a file named like an option is analysed as that file" "1" \
  "$(grep -c "^function${tab}evil${tab}--globs=z.ts:1-3" <<<"$changed" || :)"
check "binary, mode-only and empty files are listed, not dropped" \
  "blob.bin${tab}binary
empty.ts${tab}no content lines changed (mode-only, or an empty file)
mode.ts${tab}no content lines changed (mode-only, or an empty file)" \
  "$(section "not analyzed" "$out")"
check "pathspecs are relative to the current directory" \
  "function${tab}f${tab}sub/n.py:1-2${tab}yes" "$(cd sub && section changed "$("$SYMS" -- n.py)")"
printf 'x\n' > root-untracked.ts
check "untracked files outside the current directory are still named" "1" \
  "$(cd sub && "$SYMS" | grep -cx 'root-untracked.ts' || :)"
rm root-untracked.ts
git add -A && git commit -q -m second

# A class decorator must not reach the class's last method, which ends on the same byte.
printf '@dataclass\nclass Foo:\n    x: int\n\n    def last(self):\n        return 1\n' > d.py
# A content line that reads like a /dev/null header once git prefixes it.
printf 'def g():\n    return 1\n\nNOTE = """\n-- /dev/null\n"""\n' > h.py
git add . && git commit -q -m third
sed 's/return 1/return 2/' d.py > d.py.new && mv d.py.new d.py
printf 'NOTE = """\n"""\n' > h.py
out="$("$SYMS")"
check "a decorated class's last method is credited alone, as a method" \
  "method${tab}last${tab}d.py:5-6${tab}no" "$(section changed "$out" | grep 'd\.py' || :)"
check "a content line that looks like a header does not hide a removal" \
  "function${tab}g${tab}h.py:1-2${tab}yes" "$(section "removed (old-side location)" "$out" | grep 'h\.py' || :)"

# The user's diff config must not change what the parser reads.
expected="$(section changed "$out")"
for cfg in diff.mnemonicPrefix=true diff.noprefix=true diff.relative=true; do
  check "$cfg does not change the result" "$expected" \
    "$(cd sub && section changed "$(GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0="${cfg%%=*}" GIT_CONFIG_VALUE_0="${cfg#*=}" "$SYMS")")"
done
git add -A && git commit -q -m fourth

# A file ast-grep cannot read prints [] and exits 0; it must not read as "no declarations".
printf '# caf\xe9\ndef g():\n    return 1\n' > l.py
git add l.py && git commit -q -m latin1
printf '# caf\xe9\ndef g():\n    return 2\n' > l.py
check "a file that is not UTF-8 is listed, not reported as top-level" \
  "l.py${tab}not valid UTF-8" "$(section "not analyzed" "$("$SYMS")")"

# Third repository: config, attributes and file types that could shift what the parser reads.
git init -q -b main "$WORK/three"
cd "$WORK/three"
git config user.email ci@example.invalid
git config user.name CI
printf 'function f(){\n  return 1}\nfunction h(){\n\n  return 0}\nfunction g(){\n  return 1}\n' > x.ts
printf '// header\n\n\nfunction a(){\n  return 1\n}\nfunction b(){\n  return 1\n}\n' > t.ts
printf 't.ts diff=strip\n' > .gitattributes
printf 'class O1:\n    class Inner:\n        def keep(self): pass\n        def m(self): pass\n\nclass O2:\n    class Inner:\n        def m(self): pass\n' > n.py
printf 'class C {\n  get x() {\n    return 1;\n  }\n  set x(v) {\n    this.y = v;\n  }\n}\n' > c.ts
printf 'export function target() {\n  return 1;\n}\n' > real.ts
git add . && git commit -q -m base

sed '2s/1/2/;7s/1/2/' x.ts > x.new && mv x.new x.ts
sed '8s/1/2/' t.ts > t.new && mv t.new t.ts
sed '4d' n.py > n.new && mv n.new n.py
sed '5,7d' c.ts > c.new && mv c.new c.ts
ln -s real.ts link.ts && ln -s missing.ts dangling.ts && git add link.ts dangling.ts
out="$(GIT_CONFIG_COUNT=2 GIT_CONFIG_KEY_0=diff.interHunkContext GIT_CONFIG_VALUE_0=10 \
  GIT_CONFIG_KEY_1=diff.strip.textconv GIT_CONFIG_VALUE_1='sed 1,3d' "$SYMS" 2>&1)"
changed="$(section changed "$out")"
removed="$(section "removed (old-side location)" "$out")"
check "diff.interHunkContext does not pull in the unchanged function between two hunks" \
  "f g" "$(grep 'x\.ts' <<<"$changed" | cut -f2 | tr '\n' ' ' | sed 's/ $//')"
check "a textconv filter does not shift which function a line is credited to" \
  "b" "$(grep 't\.ts' <<<"$changed" | cut -f2)"
check "symlinks are listed, never read through to their target" \
  "dangling.ts${tab}symlink
link.ts${tab}symlink" "$(section "not analyzed" "$out")"
check "a method removed under one of two same-named nested classes is a removal" \
  "method${tab}m${tab}n.py:4-4${tab}no" "$(grep 'n\.py' <<<"$removed" || :)"
check "a removed setter is a removal though its getter keeps the name" \
  "method${tab}x${tab}c.ts:5-7${tab}no" "$(grep 'c\.ts' <<<"$removed" || :)"
rc=0; "$SYMS" "" >/dev/null 2>&1 || rc=$?
check "an empty range argument fails rather than meaning no range" "1" "$rc"
# Edit the surviving getter too: a line diff cannot say which x went, so neither is asserted.
sed '3s/1/2/' c.ts > c.new && mv c.new c.ts
check "a removal that could be either of two same-named members is marked ambiguous" \
  "method${tab}x${tab}c.ts:2-4${tab}no${tab}ambiguous: 1 of these 2 removed
method${tab}x${tab}c.ts:5-7${tab}no${tab}ambiguous: 1 of these 2 removed" \
  "$(section "removed (old-side location)" "$("$SYMS" 2>/dev/null)" | grep 'c\.ts' || :)"

rc=0; "$SYMS" no-such-ref >/dev/null 2>&1 || rc=$?
check "a bad range fails" "1" "$rc"
rc=0; (cd "$WORK" && "$SYMS") >/dev/null 2>&1 || rc=$?
check "outside a repository fails" "1" "$rc"
rc=0; out="$(PATH=/usr/bin:/bin "$SYMS" 2>&1)" || rc=$?
check "missing ast-grep fails rather than printing an empty result" "1:1" \
  "$rc:$(grep -c 'ast-grep is not on PATH' <<<"$out" || :)"

[ "$failures" -eq 0 ] || { echo "$failures check(s) failed" >&2; exit 1; }
echo "all checks passed"
