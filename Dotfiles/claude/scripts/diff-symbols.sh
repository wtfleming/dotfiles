#!/usr/bin/env bash
# Map a diff's changed lines to the named declarations that enclose them, so a review agent
# can be handed "these functions, classes and consts changed" instead of raw hunks. Nothing
# calls it yet; todo.org has the plan for wiring it into the reviewer.
#
# Usage:
#   diff-symbols.sh [<range>] [-- <path>...]
#     <range>  A..B, A...B (from the merge base), or a single rev (against the working
#              tree). Omitted: uncommitted changes against HEAD.
#     <path>   pathspecs, relative to the current directory as git takes them.
#
# Evidence, not verdicts: it reports what changed and where, and judges nothing. A file it
# could not analyse is listed as such, never dropped -- "no symbols" and "could not look"
# must not read the same. Rules are passed inline and ast-grep runs on copies in a scratch
# directory, so no config from the repo under analysis shapes its own analysis.
#
# Known blind spot: a parse tree-sitter repaired by inserting a missing token (an unclosed
# brace mid-edit) has no ERROR node, and ast-grep rules cannot match the MISSING node, so
# such a file is analysed as if it parsed.
#
# Written for bash 3.2 (the macOS system bash), so no associative arrays, no mapfile.

set -Eeuo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

for tool in git jq ast-grep iconv; do
  command -v "$tool" >/dev/null 2>&1 || die "$tool is not on PATH"
done
top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git working tree"

range=""
paths=()
while [ $# -gt 0 ]; do
  case $1 in
    --) shift; paths=("$@"); break ;;
    -*) die "unknown option: $1" ;;
    "") die "empty range argument" ;;
    *) [ -z "$range" ] || die "one range only, got '$range' and '$1'"; range=$1; shift ;;
  esac
done

commit() {
  git rev-parse --verify -q "$1^{commit}" || die "not a commit: $1"
}

new_sha=""   # empty means the working tree
case $range in
  "") old_sha="$(commit HEAD)" ;;
  *...*)
    a=${range%%...*}; b=${range#*...}
    old_sha="$(git merge-base "$(commit "${a:-HEAD}")" "$(commit "${b:-HEAD}")")" \
      || die "no merge base for $range"
    new_sha="$(commit "${b:-HEAD}")" ;;
  *..*)
    a=${range%%..*}; b=${range#*..}
    old_sha="$(commit "${a:-HEAD}")"; new_sha="$(commit "${b:-HEAD}")" ;;
  *) old_sha="$(commit "$range")" ;;
esac

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Every flag here pins something user config or .gitattributes could otherwise change under
# the parser: a/ and b/ on repo-root paths, hunks that hold changed lines only, line
# numbers that count the raw file ast-grep is about to read, and a submodule shown as one
# entry of its own rather than hidden, summarised or expanded into its files.
diff_cmd=(git -c core.quotePath=false diff -U0 --inter-hunk-context=0 --no-renames --no-color
  --no-ext-diff --no-textconv --src-prefix=a/ --dst-prefix=b/ --no-relative
  --submodule=short --ignore-submodules=none "$old_sha")
[ -z "$new_sha" ] || diff_cmd+=("$new_sha")
diff_cmd+=(-- ${paths[@]+"${paths[@]}"})

echo "# ${diff_cmd[*]}"
echo "# new side: ${new_sha:-working tree}; renames appear as a deletion plus an addition"

"${diff_cmd[@]}" > "$WORK/diff" || die "git diff failed"

# Records, one file at a time: F and the repo path (taken from the `diff --git` line, which
# every entry has -- binary, empty and mode-only changes have no ---/+++ lines), then A
# (added) or D (deleted), B (binary), L (symlink) and M (submodule) where they apply,
# each touched new-side line (N), and each deleted old-side line (O) -- twice, also as X,
# when its hunk adds nothing, so a deletion can be told from the old half of an edit.
# --no-renames makes both paths on the `diff --git` line the same, so the path is half of
# what follows "a/".
awk '
  /^diff --git a\// { rest = substr($0, 14); print "F\t" substr(rest, 1, (length(rest) - 3) / 2); head = 1; next }
  /^diff --git / { print "F\t" substr($0, 12); print "Q"; head = 1; next }   # git quoted an unusual path
  # header lines only: a content line can read "--- /dev/null" once git adds its prefix
  head && /^--- \/dev\/null$/ { print "A"; next }
  head && /^\+\+\+ \/dev\/null$/ { print "D"; next }
  head && /^Binary files / { print "B"; next }
  # the blob of a symlink is its target path, not source, and reading it follows the link
  head && /^(new file mode|deleted file mode|old mode|new mode) 120000$/ { print "L"; next }
  head && /^index [0-9a-f]+\.\.[0-9a-f]+ 120000$/ { print "L"; next }
  # a submodule is a commit id, whatever its name ends in
  head && /^(new file mode|deleted file mode|old mode|new mode) 160000$/ { print "M"; next }
  head && /^index [0-9a-f]+\.\.[0-9a-f]+ 160000$/ { print "M"; next }
  /^@@ / {
    head = 0
    # @@ -a[,b] +c[,d] @@ -- an omitted count is 1
    split($2, o, ","); split($3, n, ",")
    a = substr(o[1], 2) + 0; b = (2 in o) ? o[2] + 0 : 1
    c = substr(n[1], 2) + 0; d = (2 in n) ? n[2] + 0 : 1
    for (i = 0; i < b; i++) { print "O\t" a + i; if (d == 0) print "X\t" a + i }
    for (i = 0; i < d; i++) print "N\t" c + i
  }
' "$WORK/diff" > "$WORK/changes"

# Label:node-kind pairs per ast-grep language. Only top-level variable declarators count:
# a local `const` inside a function would otherwise out-rank the function that holds it.
kinds_for() {
  case $1 in
    ts|tsx) echo "function:function_declaration generator:generator_function_declaration class:class_declaration abstract_class:abstract_class_declaration method:method_definition interface:interface_declaration type:type_alias_declaration enum:enum_declaration variable:variable_declarator" ;;
    js) echo "function:function_declaration generator:generator_function_declaration class:class_declaration method:method_definition variable:variable_declarator" ;;
    python) echo "function:function_definition class:class_definition" ;;
  esac
}

rules_for() {
  local lang=$1 pair label kind sep=""
  for pair in $(kinds_for "$lang"); do
    label=${pair%%:*}; kind=${pair#*:}
    # shellcheck disable=SC2016 # $NAME is an ast-grep metavariable, not a shell one
    printf '%sid: %s\nlanguage: %s\nrule:\n  kind: %s\n  has: {field: name, pattern: $NAME}\n' \
      "$sep" "$label" "$lang" "$kind"
    if [ "$label" = variable ]; then
      printf '  inside:\n    stopBy: neighbor\n    any: [{kind: lexical_declaration}, {kind: variable_declaration}]\n    inside:\n      stopBy: neighbor\n      any: [{kind: program}, {kind: export_statement}]\n'
    fi
    sep=$'---\n'
  done
  if [ "$lang" = python ]; then
    # shellcheck disable=SC2016 # $DEF is an ast-grep metavariable
    printf -- '---\nid: decorated\nlanguage: %s\nrule:\n  kind: decorated_definition\n  has: {field: definition, pattern: $DEF}\n' "$lang"
  else
    # What an export statement can declare outright, and the unnamed things it can wrap.
    local named="" unnamed="{kind: function_expression}, {kind: arrow_function}, {kind: class}, {kind: generator_function}"
    for pair in $(kinds_for "$lang"); do
      case ${pair%%:*} in method|variable) ;; *) named="$named{kind: ${pair#*:}}, " ;; esac
    done
    named=${named%, }
    printf -- '---\nid: direct\nlanguage: %s\nrule:\n  any: [%s]\n  inside: {kind: export_statement, stopBy: neighbor}\n' "$lang" "$named"
    printf -- '---\nid: directvar\nlanguage: %s\nrule:\n  kind: variable_declarator\n  inside:\n    stopBy: neighbor\n    any: [{kind: lexical_declaration}, {kind: variable_declaration}]\n    inside: {kind: export_statement, stopBy: neighbor}\n' "$lang"
    # An export statement that declares nothing itself exports by name or by value:
    # `export { a }`, `export default a`, `export default { a }`, `export * from`.
    local wraps="$named, {kind: lexical_declaration}, {kind: variable_declaration}, $unnamed"
    # TypeScript only: overload signatures, `export declare`, and namespaces, which also
    # hold declarations the module does not export.
    if [ "$lang" != js ]; then
      wraps="$wraps, {kind: function_signature}, {kind: ambient_declaration}, {kind: internal_module}, {kind: module}"
      unnamed="$unnamed, {kind: internal_module}, {kind: module}"
    fi
    printf -- '---\nid: indirect\nlanguage: %s\nrule:\n  kind: export_statement\n  not:\n    has:\n      any: [%s]\n' "$lang" "$wraps"
    printf -- '---\nid: cjs\nlanguage: %s\nrule: {kind: member_expression, regex: "^(module[.]exports|exports[.])"}\n' "$lang"
    # Unnamed functions and classes a declaration can sit inside: tracked for nesting,
    # never printed.
    printf -- '---\nid: scope\nlanguage: %s\nrule: {any: [%s]}\n' "$lang" "$unnamed"
  fi
  printf -- '---\nid: error\nlanguage: %s\nrule: {kind: ERROR}\n' "$lang"
}

# Declarations in one file as TSV: kind, name, start line, end line, exported, start byte,
# end byte (the bytes tell nesting apart on a line several declarations share), dotted
# path of the enclosing declarations. Prints ERROR instead when the parse has error
# nodes, since symbols from a broken parse are guesses, and ENCODING when the file is not
# UTF-8: ast-grep then prints [] and exits 0, which would read as a file with no
# declarations. Prints LONGLINES for a minified or generated file: every match carries the
# text of its line, so thousands of declarations on one line cost quadratic time and memory.
#
# One sweep in source order with a stack of open declarations finds each parent, so the
# cost stays linear in a generated file of thousands of types. A Python definition starts
# at its decorators, and a method is a Python function whose parent is a class.
#
# Exported: nested for anything inside another declaration or an unnamed function or
# class -- a method of an exported class is public, so "no" would mislead. Python: yes
# unless the name starts with an underscore. TS/JS: yes when an export statement declares
# it outright, as ast-grep reports it rather than by position. Otherwise no -- or unknown
# when the file also exports by a route this does not resolve: `export { a }`,
# `export default a`, `export * from`, CommonJS. Best effort for TypeScript, which has
# more export forms than are listed here; an unlisted one reads unknown.
decls() {
  local file=$1 lang=$2
  iconv -f UTF-8 -t UTF-8 "$file" >/dev/null 2>&1 || { echo ENCODING; return 0; }
  LC_ALL=C awk 'length > 5000 { exit 1 }' "$file" || { echo LONGLINES; return 0; }
  # ./ so a file named like an option reaches ast-grep as a file
  ( cd "$(dirname "$file")" && ast-grep scan --inline-rules "$(rules_for "$lang")" --json=compact "./$(basename "$file")" ) \
    | jq -r --arg lang "$lang" '
      [ .[] | { id: .ruleId, name: (.metaVariables.single.NAME.text // ""),
                s: (.range.start.line + 1), e: (.range.end.line + 1),
                sb: .range.byteOffset.start, eb: .range.byteOffset.end,
                def: .metaVariables.single.DEF.range.byteOffset.start } ] as $m
      | if any($m[]; .id == "error") then "ERROR" else
        # keyed on where the wrapped definition starts: a class and its last method share
        # an end byte, so the end would hand the class decorator to the method
        ($m | map(select(.id == "decorated")) | map({ key: (.def | tostring), value: . }) | from_entries) as $dec
        | ($m | map(select(.id == "direct" or .id == "directvar")) | map({ key: "\(.sb):\(.eb)", value: true }) | from_entries) as $direct
        | any($m[]; .id == "indirect" or .id == "cjs") as $indirect
        | [ foreach ($m
              | map(select(.id | IN("decorated", "error", "direct", "directvar", "indirect", "cjs") | not)
                    | ($dec[.sb | tostring]) as $d
                    | if $d != null then .s = $d.s | .sb = $d.sb else . end
                    | if .id == "scope" then .name = "<unnamed>" else . end)
              | sort_by(.sb, -.eb))[] as $x ({ stack: [], row: null };
              .stack |= until(length == 0 or (.[-1].eb >= $x.eb and (.[-1].sb != $x.sb or .[-1].eb != $x.eb)); .[:-1])
              | .row = { x: $x, p: (.stack[-1] // null), path: (.stack | map(.name) | join(".")) }
              | .stack += [$x];
              .row) ]
        | .[] | select(.x.id != "scope")
        | (if $lang == "python" and .x.id == "function" and .p.id == "class" then "method" else .x.id end) as $kind
        | (if .p != null then "nested"
           elif $lang == "python" then (if (.x.name | startswith("_")) then "no" else "yes" end)
           elif $direct["\(.x.sb):\(.x.eb)"] then "yes"
           elif $indirect then "unknown" else "no" end) as $exported
        | [ $kind, .x.name, .x.s, .x.e, $exported, .x.sb, .x.eb, .path ] | @tsv
        end'
}

lang_of() {
  case $1 in
    *.ts) echo ts ;; *.tsx) echo tsx ;; *.js) echo js ;; *.py) echo python ;; *) echo "" ;;
  esac
}

# Write one side of a file into the scratch directory under its own extension. The copy
# has ast-grep's suppression comment defused, same length so no offset moves: a file or a
# declaration marked `ast-grep-ignore` would otherwise match no rule here, the parse-error
# rule included, and read as having no declarations.
side() {
  local which=$1 rev=$2 path=$3 out
  out="$WORK/$which/$path"
  mkdir -p "$(dirname "$out")"
  # the status matters: callers run this in $(...), where errexit is off, and a read that
  # failed would otherwise leave an empty copy that parses as a file with no declarations
  if [ -z "$rev" ]; then cat "$top/$path"; else git show "$rev:$path"; fi \
    | LC_ALL=C sed 's/ast-grep-ignore/ast-grep-ignorX/g' > "$out" || return 1
  echo "$out"
}

# For each line record in file-lines with one of the given tags, print the innermost
# declarations that hold it: kind, name, path:start-end, exported, enclosing path, tag --
# or "-", path:line and the tag when none does. A line can be held by several innermost
# declarations (`const a = 1, b = 2`), and a line diff cannot say which of them changed,
# so all of them are printed.
#
# The line records are sorted once and each declaration visits only the records inside
# its own range (found by binary search), so the cost is lines times nesting depth rather
# than lines times declarations.
enclosing() {
  awk -F'\t' -v tags="$2" 'index(tags, $1)' "$WORK/file-lines" | sort -u | sort -t "$(printf '\t')" -k2,2n > "$WORK/records"
  awk -F'\t' -v OFS='\t' -v path="$3" '
    FILENAME == ARGV[1] { r++; tg[r] = $1; ln[r] = $2; next }
    {
      i = FNR; k[i] = $1; n[i] = $2; s[i] = $3; e[i] = $4; x[i] = $5; sb[i] = $6; eb[i] = $7; p[i] = $8
      a = 1; b = r + 1
      while (a < b) { m = int((a + b) / 2); if (ln[m] < s[i]) a = m + 1; else b = m }
      for (j = a; j <= r && ln[j] <= e[i]; j++) cand[j, ++nc[j]] = i
    }
    END {
      for (j = 1; j <= r; j++) {
        hit = 0
        for (a = 1; a <= nc[j]; a++) {
          ca = cand[j, a]; inner = 1
          for (b = 1; b <= nc[j]; b++) {
            cb = cand[j, b]
            if (a != b && sb[ca] <= sb[cb] && eb[cb] <= eb[ca] && (sb[ca] != sb[cb] || eb[ca] != eb[cb])) inner = 0
          }
          if (inner) { print k[ca], n[ca], path ":" s[ca] "-" e[ca], x[ca], p[ca], tg[j]; hit = 1 }
        }
        if (!hit) print "-", path ":" ln[j], tg[j]
      }
    }
  ' "$WORK/records" "$1"
}

: > "$WORK/changed"; : > "$WORK/toplevel"; : > "$WORK/removed"; : > "$WORK/skipped"
files=0

analyse() {
  local path=$1 added=$2 deleted=$3 skip=$4 lang nf of
  [ -n "$path" ] || return 0
  lang="$(lang_of "$path")"
  if [ -n "$skip" ]; then
    printf '%s\t%s\n' "$path" "$skip" >> "$WORK/skipped"; return 0
  elif ! grep -q . "$WORK/file-lines"; then
    printf '%s\tno content lines changed (mode-only, or an empty file)\n' "$path" >> "$WORK/skipped"; return 0
  elif [ -z "$lang" ]; then
    printf '%s\tunsupported language\n' "$path" >> "$WORK/skipped"; return 0
  fi

  : > "$WORK/new.tsv"; : > "$WORK/old.tsv"
  if [ "$deleted" = 0 ]; then
    nf="$(side new "$new_sha" "$path")" || { printf '%s\tcould not read the new side\n' "$path" >> "$WORK/skipped"; return 0; }
    decls "$nf" "$lang" > "$WORK/new.tsv" || { printf '%s\tast-grep failed\n' "$path" >> "$WORK/skipped"; return 0; }
  fi
  if [ "$added" = 0 ]; then
    of="$(side old "$old_sha" "$path")" || { printf '%s\tcould not read the old side\n' "$path" >> "$WORK/skipped"; return 0; }
    decls "$of" "$lang" > "$WORK/old.tsv" || { printf '%s\tast-grep failed\n' "$path" >> "$WORK/skipped"; return 0; }
  fi
  if grep -qx ENCODING "$WORK/new.tsv" "$WORK/old.tsv"; then
    printf '%s\tnot valid UTF-8\n' "$path" >> "$WORK/skipped"; return 0
  fi
  if grep -qx LONGLINES "$WORK/new.tsv" "$WORK/old.tsv"; then
    printf '%s\tvery long lines (minified or generated)\n' "$path" >> "$WORK/skipped"; return 0
  fi
  if grep -qx ERROR "$WORK/new.tsv" "$WORK/old.tsv"; then
    printf '%s\tparse errors\n' "$path" >> "$WORK/skipped"; return 0
  fi

  : > "$WORK/credited"
  if [ "$deleted" = 0 ]; then
    enclosing "$WORK/new.tsv" N "$path" | awk -F'\t' -v OFS='\t' \
        -v changed="$WORK/changed" -v top="$WORK/toplevel" -v credited="$WORK/credited" '
      $1 == "-" { print $2 >> top; next }
      { print $1, $2, $3, $4 >> changed; print $1, $5, $2, $3 >> credited }
    '
  else
    printf '%s (file deleted)\n' "$path" >> "$WORK/toplevel"
  fi
  # Deleted lines are credited from the old side, where they still exist. The declaration
  # that held one is removed if fewer of its kind and name are left under the same
  # enclosing path than there were -- a count, because a getter and its setter share all
  # three. Otherwise it changed, and is credited at its new-side location, unless the
  # new-side pass already credited that key: the old half of an edit says nothing new.
  # Wherever several declarations share the key, a line diff cannot say which one is
  # meant, so each row says so rather than claiming it. A line deleted outright from
  # outside any declaration is top-level.
  enclosing "$WORK/old.tsv" OX "$path" | awk -F'\t' -v OFS='\t' -v path="$path" -v deleted="$deleted" \
      -v changed="$WORK/changed" -v top="$WORK/toplevel" -v removed="$WORK/removed" '
    FILENAME == ARGV[1] { credited[$1 "\t" $2 "\t" $3] = 1; printed[$0] = 1; next }
    FILENAME == ARGV[2] {
      key = $1 "\t" $8 "\t" $2; new[key]++; at[key, new[key]] = path ":" $3 "-" $4
      loc[key, new[key]] = $1 OFS $2 OFS at[key, new[key]] OFS $5; next
    }
    FILENAME == ARGV[3] { old[$1 "\t" $8 "\t" $2]++; next }
    $1 == "-" { if ($3 == "X" && deleted == 0) print $2 " (old side, deleted)" >> top; next }
    {
      key = $1 "\t" $5 "\t" $2; row = $1 OFS $2 OFS $3 OFS $4
      if (old[key] > new[key]) { if (!(row in seen)) { seen[row] = 1; rows[key, ++cnt[key]] = row } }
      else if ($6 == "X" || !(key in credited))
        for (i = 1; i <= new[key]; i++) {
          if (new[key] == 1) print loc[key, i] >> changed
          else if (!((key "\t" at[key, i]) in printed))
            print loc[key, i], "ambiguous: lines deleted from this or a same-named sibling" >> changed
        }
    }
    END {
      for (key in cnt) {
        gone = old[key] - new[key]
        for (i = 1; i <= cnt[key]; i++)
          print rows[key, i] (cnt[key] > gone ? OFS "ambiguous: " gone " of these " cnt[key] " removed" : "") >> removed
      }
    }
  ' "$WORK/credited" "$WORK/new.tsv" "$WORK/old.tsv" -
}

path="" added=0 deleted=0 skip=""
: > "$WORK/file-lines"
while IFS=$'\t' read -r tag value; do
  case $tag in
    F)
      analyse "$path" "$added" "$deleted" "$skip"
      path=$value added=0 deleted=0 skip="" files=$((files + 1))
      : > "$WORK/file-lines" ;;
    A) added=1 ;;
    D) deleted=1 ;;
    B) skip=binary ;;
    L) skip=symlink ;;
    M) skip=submodule ;;
    Q) skip="path git had to quote" ;;
    *) printf '%s\t%s\n' "$tag" "$value" >> "$WORK/file-lines" ;;
  esac
done < "$WORK/changes"
analyse "$path" "$added" "$deleted" "$skip"

section() {
  echo
  echo "$1:"
  if [ -s "$2" ]; then sort -u "$2"; else echo "(none)"; fi
}

if [ "$files" -eq 0 ]; then
  echo
  echo "no changes in this diff"
else
  echo "# columns: kind, name, file:start-end, exported (yes, no, nested or unknown)[, note]"
  section changed "$WORK/changed"
  section top-level "$WORK/toplevel"
  section "removed (old-side location)" "$WORK/removed"
  section "not analyzed" "$WORK/skipped"
fi

if [ -z "$new_sha" ]; then
  # With no pathspec git diff covers the whole repo, but ls-files only the current directory.
  [ ${#paths[@]} -gt 0 ] || paths=(:/)
  git ls-files --others --exclude-standard --full-name -- "${paths[@]}" > "$WORK/untracked"
  [ ! -s "$WORK/untracked" ] || section "untracked (not in the diff)" "$WORK/untracked"
fi
