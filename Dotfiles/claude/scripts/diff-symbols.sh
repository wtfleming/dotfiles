#!/usr/bin/env bash
# Map a diff's changed lines to the named declarations that enclose them, so a review agent
# is handed "these functions, classes and consts changed" instead of raw hunks.
#
# Usage:
#   diff-symbols.sh [<range>] [-- <path>...]
#     <range>  A..B, A...B (from the merge base), or a single rev (against the working
#              tree). Omitted: uncommitted changes against HEAD.
#
# Evidence, not verdicts: it reports what changed and where, and judges nothing. A file it
# could not analyse is listed as such, never dropped -- "no symbols" and "could not look"
# must not read the same. Rules are passed inline and ast-grep runs on copies in a scratch
# directory, so no config from the repo under analysis shapes its own analysis.
#
# Written for bash 3.2 (the macOS system bash), so no associative arrays, no mapfile.

set -Eeuo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

for tool in git jq ast-grep; do
  command -v "$tool" >/dev/null 2>&1 || die "$tool is not on PATH"
done
top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git working tree"
cd "$top"

range=""
paths=()
while [ $# -gt 0 ]; do
  case $1 in
    --) shift; paths=("$@"); break ;;
    -*) die "unknown option: $1" ;;
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

diff_cmd=(git -c core.quotePath=false diff -U0 --no-renames --no-color --no-ext-diff "$old_sha")
[ -z "$new_sha" ] || diff_cmd+=("$new_sha")
diff_cmd+=(-- ${paths[@]+"${paths[@]}"})

echo "# ${diff_cmd[*]}"
echo "# new side: ${new_sha:-working tree}; renames appear as a deletion plus an addition"

"${diff_cmd[@]}" > "$WORK/diff" || die "git diff failed"

# One record per file (F, old path, new path), then each touched new-side line (N) and
# each deleted old-side line (O). A pure deletion touches the new-side line before it.
awk '
  /^diff --git / { infile = 1; next }
  infile && /^--- / { old = substr($0, 5); sub(/^a\//, "", old); next }
  infile && /^\+\+\+ / {
    new = substr($0, 5); sub(/^b\//, "", new)
    printf "F\t%s\t%s\n", old, new; infile = 0; next
  }
  /^@@ / {
    # @@ -a[,b] +c[,d] @@ -- an omitted count is 1
    split($2, o, ","); split($3, n, ",")
    a = substr(o[1], 2) + 0; b = (2 in o) ? o[2] + 0 : 1
    c = substr(n[1], 2) + 0; d = (2 in n) ? n[2] + 0 : 1
    for (i = 0; i < b; i++) printf "O\t%d\n", a + i
    if (new != "/dev/null") {
      if (d == 0) printf "N\t%d\n", (c > 0 ? c : 1)
      for (i = 0; i < d; i++) printf "N\t%d\n", c + i
    }
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
  [ "$lang" = python ] || printf -- '---\nid: export\nlanguage: %s\nrule: {kind: export_statement}\n' "$lang"
  printf -- '---\nid: error\nlanguage: %s\nrule: {kind: ERROR}\n' "$lang"
}

# Declarations in one file as TSV: kind, name, start line, end line, exported, start byte,
# end byte (the bytes tell nesting apart on a line several declarations share). Prints
# ERROR instead when the parse has error nodes, since symbols from a broken parse are
# guesses. A method is a function whose nearest enclosing declaration is a class; a
# declaration is exported when nothing encloses it and it sits in an export statement
# (TS/JS) or its name has no leading underscore (Python).
decls() {
  local file=$1 lang=$2 dir
  dir="$(dirname "$file")"
  ( cd "$dir" && ast-grep scan --inline-rules "$(rules_for "$lang")" --json=compact "$(basename "$file")" ) \
    | jq -r --arg lang "$lang" '
      [ .[] | { id: .ruleId, name: (.metaVariables.single.NAME.text // ""),
                s: (.range.start.line + 1), e: (.range.end.line + 1),
                sb: .range.byteOffset.start, eb: .range.byteOffset.end } ] as $m
      | def within($o; $i): $o.sb <= $i.sb and $i.eb <= $o.eb and ($o.sb != $i.sb or $o.eb != $i.eb);
        if any($m[]; .id == "error") then "ERROR" else
        ($m | map(select(.id == "export"))) as $exports
        | ($m | map(select(.id != "export" and .id != "error"))) as $d
        | $d[] as $x
        | ([ $d[] | select(within(.; $x)) ] | sort_by(.eb - .sb) | first) as $parent
        | (if $lang == "python" and $x.id == "function" and $parent.id == "class" then "method" else $x.id end) as $kind
        | (if $parent != null then "no"
           elif $lang == "python" then (if ($x.name | startswith("_")) then "no" else "yes" end)
           elif any($exports[]; within(.; $x)) then "yes" else "no" end) as $exported
        | [ $kind, $x.name, $x.s, $x.e, $exported, $x.sb, $x.eb ] | @tsv
        end'
}

lang_of() {
  case $1 in
    *.ts) echo ts ;; *.tsx) echo tsx ;; *.js) echo js ;; *.py) echo python ;; *) echo "" ;;
  esac
}

# Write one side of a file into the scratch directory under its own extension.
side() {
  local which=$1 rev=$2 path=$3 out
  out="$WORK/$which/$path"
  mkdir -p "$(dirname "$out")"
  if [ -z "$rev" ]; then cp "$top/$path" "$out"; else git show "$rev:$path" > "$out"; fi
  echo "$out"
}

: > "$WORK/changed"; : > "$WORK/toplevel"; : > "$WORK/removed"; : > "$WORK/skipped"
files=0

analyse() {
  local old=$1 new=$2 path lang nf of
  [ -n "$old$new" ] || return 0
  path=$new; [ "$path" != /dev/null ] || path=$old
  lang="$(lang_of "$path")"
  if [ -z "$lang" ]; then
    printf '%s\tunsupported language\n' "$path" >> "$WORK/skipped"; return 0
  fi

  : > "$WORK/new.tsv"; : > "$WORK/old.tsv"
  if [ "$new" != /dev/null ]; then
    nf="$(side new "$new_sha" "$new")"
    decls "$nf" "$lang" > "$WORK/new.tsv" || { printf '%s\tast-grep failed\n' "$path" >> "$WORK/skipped"; return 0; }
  fi
  if [ "$old" != /dev/null ]; then
    of="$(side old "$old_sha" "$old")"
    decls "$of" "$lang" > "$WORK/old.tsv" || { printf '%s\tast-grep failed\n' "$path" >> "$WORK/skipped"; return 0; }
  fi
  if grep -qx ERROR "$WORK/new.tsv" "$WORK/old.tsv"; then
    printf '%s\tparse errors\n' "$path" >> "$WORK/skipped"; return 0
  fi

  enclosing "$WORK/new.tsv" N "$new" | while IFS=$'\t' read -r k rest; do
    if [ "$k" = - ]; then echo "$rest" >> "$WORK/toplevel"; else printf '%s\t%s\n' "$k" "$rest" >> "$WORK/changed"; fi
  done
  # A declaration enclosing a deleted line is removed if its kind and name are gone.
  enclosing "$WORK/old.tsv" O "$old" | awk -F'\t' '
    FILENAME == ARGV[1] { present[$1 "\t" $2] = 1; next }
    $1 != "-" && !(($1 "\t" $2) in present)
  ' "$WORK/new.tsv" - >> "$WORK/removed"
}

# For each line tagged <tag> in file-lines, print the innermost declarations whose line
# range holds it, as kind, name, path:start-end, exported -- or "-" and path:line when
# none does. A line can be held by several innermost declarations (`const a = 1, b = 2`),
# and a line diff cannot say which of them changed, so all of them are printed.
enclosing() {
  awk -F'\t' -v OFS='\t' -v tag="$2" -v path="$3" '
    FILENAME == ARGV[1] { k[FNR] = $1; n[FNR] = $2; s[FNR] = $3; e[FNR] = $4; x[FNR] = $5; sb[FNR] = $6; eb[FNR] = $7; cnt = FNR; next }
    $1 == tag {
      m = 0
      for (i = 1; i <= cnt; i++) if (s[i] <= $2 && $2 <= e[i]) c[++m] = i
      hit = 0
      for (a = 1; a <= m; a++) {
        inner = 1
        for (b = 1; b <= m; b++)
          if (a != b && sb[c[a]] <= sb[c[b]] && eb[c[b]] <= eb[c[a]] && (sb[c[a]] != sb[c[b]] || eb[c[a]] != eb[c[b]])) inner = 0
        if (inner) { i = c[a]; print k[i], n[i], path ":" s[i] "-" e[i], x[i]; hit = 1 }
      }
      if (!hit) print "-", path ":" $2
    }
  ' "$1" "$WORK/file-lines"
}

old_path="" new_path=""
: > "$WORK/file-lines"
while IFS=$'\t' read -r tag a b; do
  if [ "$tag" = F ]; then
    analyse "$old_path" "$new_path"
    old_path=$a; new_path=$b; files=$((files + 1))
    : > "$WORK/file-lines"
  else
    printf '%s\t%s\n' "$tag" "$a" >> "$WORK/file-lines"
  fi
done < "$WORK/changes"
analyse "$old_path" "$new_path"

section() {
  echo
  echo "$1:"
  if [ -s "$2" ]; then sort -u "$2"; else echo "(none)"; fi
}

if [ "$files" -eq 0 ]; then
  echo
  echo "no changes in this diff"
else
  echo "# columns: kind, name, file:start-end, exported"
  section changed "$WORK/changed"
  section top-level "$WORK/toplevel"
  section "removed (old-side location)" "$WORK/removed"
  section "not analyzed" "$WORK/skipped"
fi

if [ -z "$new_sha" ]; then
  git ls-files --others --exclude-standard -- ${paths[@]+"${paths[@]}"} > "$WORK/untracked"
  [ ! -s "$WORK/untracked" ] || section "untracked (not in the diff)" "$WORK/untracked"
fi
