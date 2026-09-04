#!/usr/bin/env bash
# Exercise resolve-scope.sh against a scratch repository.
#
# Everything else CI runs is static -- shellcheck, `jq empty`, a frontmatter parse -- and
# none of it can reach the branching this script is almost entirely made of. That branching
# is now the single authority on what every review, verification and design pass reads, so
# a wrong answer here is a confident report about the wrong code.
#
# Each case asserts the manifest fields the consumers actually branch on. `correspondence`
# is the highest-value one: every agent decides from it whether to read a file from disk.

set -Eeuo pipefail

RESOLVE="$(cd "$(dirname "$0")/.." && pwd)/scripts/resolve-scope.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
# Every resolve publishes under $TMPDIR, and the trap above only removes $WORK -- so
# without this the suite scatters artifact trees through the ambient temp directory and
# cleans up none of them. It also makes the republish assertions independent of whatever
# a previous run left behind.
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

# `git init -b` needs git 2.28+; the runner has it, and so does the macOS target.
scratch_repo() {
  local dir=$1 branch=${2:-main}
  git init -q -b "$branch" "$dir"
  git -C "$dir" config user.email ci@example.invalid
  git -C "$dir" config user.name CI
}

commit() {
  local dir=$1 file=$2 body=$3 msg=$4
  printf '%s\n' "$body" > "$dir/$file"
  git -C "$dir" add "$file"
  git -C "$dir" commit -q -m "$msg"
}

field() {
  jq -r "$2" "$1/manifest.json"
}

echo "== default branch is resolved, not assumed =="
scratch_repo "$WORK/trunk" trunk
commit "$WORK/trunk" a.txt one base
check "base on a trunk repo" "trunk" "$(cd "$WORK/trunk" && "$RESOLVE" base)"

echo "== the three auto fall-through steps =="
scratch_repo "$WORK/auto"
commit "$WORK/auto" a.txt one base
printf 'dirty\n' >> "$WORK/auto/a.txt"
out=$(cd "$WORK/auto" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "auto step 1 shape" "worktree" "$(field "$out" .shape)"
check "auto step 1 step" "auto-1-worktree" "$(field "$out" .resolution_step)"
check "auto step 1 correspondence" "workspace" "$(field "$out" .correspondence)"

git -C "$WORK/auto" checkout -q -- a.txt
git -C "$WORK/auto" checkout -q -b feature
commit "$WORK/auto" b.txt two "on the branch"
out=$(cd "$WORK/auto" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "auto step 2 step" "auto-2-branch" "$(field "$out" .resolution_step)"
check "auto step 2 records step 1" "1" "$(field "$out" '.fell_through | length')"

echo "== untracked work is folded into the same diff =="
scratch_repo "$WORK/untracked"
commit "$WORK/untracked" a.txt one base
printf 'new\n' > "$WORK/untracked/fresh.txt"
out=$(cd "$WORK/untracked" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "untracked file is in the scope" "fresh.txt" "$(field "$out" '.files[0]')"

echo "== ranges split on the operator, not the last dot =="
scratch_repo "$WORK/range"
commit "$WORK/range" a.txt one base
git -C "$WORK/range" tag v1.0
commit "$WORK/range" a.txt two second
git -C "$WORK/range" tag v2.0
head=$(git -C "$WORK/range" rev-parse HEAD)
for spec in 'v1.0..v2.0' 'v1.0...v2.0' 'HEAD~1..'; do
  out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope "$spec" 2>/dev/null | tail -1)
  check "range $spec resolves its head" "$head" "$(field "$out" .scope_head)"
done

echo "== a commit scope is one commit, and HEAD is a commit =="
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "HEAD is a commit, not a branch" "commit" "$(field "$out" .shape)"

echo "== correspondence tracks the checkout =="
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD~1 2>/dev/null | tail -1)
check "an older commit is scope-behind" "scope-behind" "$(field "$out" .correspondence)"
printf 'uncommitted\n' >> "$WORK/range/a.txt"
out=$(cd "$WORK/range" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "a dirty checkout is same-dirty" "same-dirty" "$(field "$out" .correspondence)"
git -C "$WORK/range" checkout -q -- a.txt

echo "== the remaining correspondence states, and the branch shape =="
# `same`, `scope-ahead` and `divergent` were the states nothing asserted, and every agent
# decides from this field whether to read a file from disk. `unknown` is not reachable
# offline -- it needs a head that resolution produced but the object database lacks, which
# only the PR shape can do -- so it stays uncovered, deliberately and on the record.
scratch_repo "$WORK/corr"
commit "$WORK/corr" a.txt one base
git -C "$WORK/corr" checkout -q -b feature
commit "$WORK/corr" b.txt two "on the branch"
out=$(cd "$WORK/corr" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "a clean checkout at the reviewed commit is same" "same" "$(field "$out" .correspondence)"

git -C "$WORK/corr" checkout -q main
out=$(cd "$WORK/corr" && "$RESOLVE" resolve --scope feature 2>/dev/null | tail -1)
check "an unmerged branch is scope-ahead" "scope-ahead" "$(field "$out" .correspondence)"
check "an explicit branch ref resolves as the branch shape" "branch" "$(field "$out" .shape)"

commit "$WORK/corr" c.txt three "on main"
out=$(cd "$WORK/corr" && "$RESOLVE" resolve --scope feature 2>/dev/null | tail -1)
check "a branch off another line is divergent" "divergent" "$(field "$out" .correspondence)"
# The note is the field advertised as ready to paste, so it must carry the head rather
# than a placeholder -- the relay hop where the Scope line is the only channel depends on it.
note=$(field "$out" .correspondence_note)
case "$note" in
  *"<scope_head>"*) echo "  FAIL  correspondence_note still carries a <scope_head> placeholder" >&2
                    failures=$((failures + 1)) ;;
  *) echo "  ok    correspondence_note names the head, not a placeholder" ;;
esac

echo "== a ref outranks a path, whatever directory the caller stands in =="
mkdir -p "$WORK/corr/sub/feature"
out=$(cd "$WORK/corr/sub" && "$RESOLVE" resolve --scope feature 2>/dev/null | tail -1)
check "a branch named like a subdirectory is still the branch" "branch" "$(field "$out" .shape)"
check "and the collision is disclosed" "1" "$(field "$out" '.warnings | length')"

echo "== --base overrides the resolved default =="
# Against a ref that is *not* the default. Passing `main` here would assert nothing: it is
# already what resolve_default_branch returns in this fixture, so an implementation that
# ignored --base entirely would still pass.
git -C "$WORK/corr" branch other-base main
out=$(cd "$WORK/corr" && "$RESOLVE" resolve --scope feature --base other-base 2>/dev/null | tail -1)
check "--base is used as the base ref" "other-base" "$(field "$out" .base_ref)"
check "and the diff is taken against it" "0" \
  "$(field "$out" .resolved_by | grep -c 'merge-base main' || true)"

echo "== auto step 3: a clean tree on the default branch falls through to HEAD =="
scratch_repo "$WORK/step3"
commit "$WORK/step3" a.txt one base
commit "$WORK/step3" a.txt two second
out=$(cd "$WORK/step3" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "auto step 3 step" "auto-3-head" "$(field "$out" .resolution_step)"
check "auto step 3 shape" "commit" "$(field "$out" .shape)"
check "auto step 3 records the two it skipped" "2" "$(field "$out" '.fell_through | length')"

echo "== a path scope picks up untracked files under it =="
scratch_repo "$WORK/pathscope"
commit "$WORK/pathscope" a.txt one base
mkdir -p "$WORK/pathscope/lib" "$WORK/pathscope/other"
printf 'in\n' > "$WORK/pathscope/lib/new.txt"
printf 'out\n' > "$WORK/pathscope/other/new.txt"
out=$(cd "$WORK/pathscope" && "$RESOLVE" resolve --scope lib 2>/dev/null | tail -1)
check "path scope shape" "path" "$(field "$out" .shape)"
check "path scope takes only what is under it" "lib/new.txt" "$(field "$out" '.files | join(",")')"

echo "== a root commit has no parent to diff against =="
scratch_repo "$WORK/root"
commit "$WORK/root" a.txt one base
out=$(cd "$WORK/root" && "$RESOLVE" resolve --scope HEAD 2>/dev/null | tail -1)
check "root commit resolves" "1" "$(field "$out" .file_count)"

echo "== a scope may not be an option, and prose is not a failure =="
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope '--output=/tmp/pwned' >/dev/null 2>&1) || rc=$?
check "an option-shaped scope is refused" "1" "$rc"
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'how login works' >/dev/null 2>&1) || rc=$?
check "prose exits 2" "2" "$rc"
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'no-such-branch' >/dev/null 2>&1) || rc=$?
check "an unresolvable token exits 1" "1" "$rc"

echo "== untracked bytes are bounded in aggregate, not just per file =="
scratch_repo "$WORK/budget"
commit "$WORK/budget" a.txt one base
# Twelve files of 1MB each: every one is at or under the per-file cap, so only the
# aggregate budget can stop them.
mkdir -p "$WORK/budget/bulk"
i=0
while [ "$i" -lt 12 ]; do
  head -c 1048576 /dev/zero | tr '\0' 'x' > "$WORK/budget/bulk/f$i.txt"
  i=$((i + 1))
done
out=$(cd "$WORK/budget" && "$RESOLVE" resolve 2>/dev/null | tail -1)
check "all twelve stay in the file list" "12" "$(field "$out" .file_count)"
bytes=$(field "$out" .diff_bytes)
if [ "$bytes" -lt 12582912 ]; then
  echo "  ok    the diff is bounded ($bytes bytes, not 12M+)"
else
  echo "  FAIL  aggregate budget did not bind: $bytes bytes" >&2
  failures=$((failures + 1))
fi

echo "== a PR URL for another repository is refused =="
# Exit 1, not 2: callers read 2 as "this is a subject" and would go looking for code that
# implements the URL. Both the no-origin and the wrong-origin paths must land on 1.
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'https://github.com/torvalds/linux/pull/1' >/dev/null 2>&1) || rc=$?
check "a foreign PR URL with no origin is a stop" "1" "$rc"
git -C "$WORK/range" remote add origin git@github.com:someone/thing.git
rc=0; (cd "$WORK/range" && "$RESOLVE" resolve --scope 'https://github.com/torvalds/linux/pull/1' >/dev/null 2>&1) || rc=$?
check "a foreign PR URL against a real origin is a stop" "1" "$rc"
git -C "$WORK/range" remote remove origin

echo "== an empty scope is never written =="
scratch_repo "$WORK/empty"
commit "$WORK/empty" a.txt one base
rc=0; (cd "$WORK/empty" && "$RESOLVE" resolve --scope main >/dev/null 2>&1) || rc=$?
check "standing on the default branch is a stop, not an empty manifest" "1" "$rc"

# Republishing is where the artifact directory stops being a value and starts being a
# shared resource: the path is keyed on repo and scope, so a second resolve of the same
# scope lands on the directory the first one's agents were handed. The old code deleted
# it; the fix flips a symlink instead, and both halves need pinning -- the swap has to
# actually take effect, and the tree it replaced has to survive.
echo "== republishing a scope neither loses the new answer nor deletes the old tree =="
scratch_repo "$WORK/republish"
commit "$WORK/republish" a.txt one base
commit "$WORK/republish" b.txt two second
commit "$WORK/republish" c.txt three third

# What the script prints is the pinned tree, not the pointer: a consumer that held the
# pointer would re-resolve it on read and get whichever run published last.
first="$(cd "$WORK/republish" && "$RESOLVE" resolve --scope 'HEAD~1...HEAD' | tail -1)"
check "the first resolve publishes its own file count" "1" "$(field "$first" '.file_count')"
check "the published path is a real directory, not a pointer" "yes" \
  "$([ -d "$first" ] && [ ! -L "$first" ] && echo yes || echo no)"
check "the manifest names the tree it was written into" "$first" \
  "$(field "$first" '.out_dir')"

# mktemp appends one suffix to $out, so stripping it back off names the pointer.
pointer="${first%.*}"
check "a pointer is left beside it for a human" "$(basename "$first")" \
  "$(readlink "$pointer" || echo "")"

# A different scope must not land on the same pointer at all -- and reading a field out
# of it cannot show that, since a collision would just serve the newer manifest. Both
# trees share a per-repo parent, so it is the pointer the comparison has to be on.
again="$(cd "$WORK/republish" && "$RESOLVE" resolve --scope 'HEAD~2...HEAD' | tail -1)"
check "a different scope gets its own directory" "no" \
  "$([ "${again%.*}" = "${first%.*}" ] && echo yes || echo no)"
check "and its own answer" "2" "$(field "$again" '.file_count')"

# Same scope string, so the same pointer: this is the collision the swap exists for.
# What it resolves to must differ, or a stale tree satisfies the assertion just as well
# as a fresh one -- two files in one commit gives a count the old manifest cannot have.
printf 'four\n' > "$WORK/republish/d.txt"
printf 'five\n' > "$WORK/republish/e.txt"
git -C "$WORK/republish" add d.txt e.txt
git -C "$WORK/republish" commit -q -m fourth

third="$(cd "$WORK/republish" && "$RESOLVE" resolve --scope 'HEAD~1...HEAD' | tail -1)"
check "republishing the same scope serves the new manifest" "2" "$(field "$third" '.file_count')"
check "on a tree of its own, not the one already handed out" "no" \
  "$([ "$third" = "$first" ] && echo yes || echo no)"

# The whole point: an agent dispatched with the first path still reads the first answer.
check "the tree it replaced still holds its own manifest" "1" "$(field "$first" '.file_count')"

check "the pointer now names the new tree" "$(basename "$third")" \
  "$(readlink "$pointer" || echo "")"

# `mv` onto an existing symlink-to-directory follows it and deposits the new link
# *inside* the old target, which leaves every later reader on the stale tree and is
# invisible from the exit status. The old tree holds its two files and nothing else.
check "the swap did not nest the new pointer inside the old tree" "manifest.json scope.diff" \
  "$(find "$first" -mindepth 1 -maxdepth 1 -exec basename {} \; | sort | tr '\n' ' ' | sed 's/ $//')"

echo "== the directory a scope is written into is validated first =="
# Both levels, because only the leaf being checked is the hole: `wtf-scope` is a fixed,
# publicly documented name, so it is the component a local user plants on a shared /tmp.
guard="$WORK/guard"
mkdir -p "$guard" "$WORK/guard-elsewhere"
ln -s "$WORK/guard-elsewhere" "$guard/wtf-scope"
rc=0
( cd "$WORK/republish" && TMPDIR="$guard" "$RESOLVE" resolve --scope HEAD ) >/dev/null 2>&1 || rc=$?
check "a symlinked wtf-scope is refused" "1" "$rc"
check "and nothing was written through it" "" "$(ls -A "$WORK/guard-elsewhere")"

# The per-repo level below it, which only exists once a resolve has created it.
guard2="$WORK/guard2"
mkdir -p "$guard2"
inner="$(cd "$WORK/republish" && TMPDIR="$guard2" "$RESOLVE" resolve --scope HEAD | tail -1)"
repo_parent="$(dirname "$inner")"
mv "$repo_parent" "$repo_parent.moved"
ln -s "$repo_parent.moved" "$repo_parent"
rc=0
( cd "$WORK/republish" && TMPDIR="$guard2" "$RESOLVE" resolve --scope HEAD ) >/dev/null 2>&1 || rc=$?
check "a symlinked per-repo parent is refused" "1" "$rc"

# Ownership alone accepts both of these: the directories are ours and only the mode is
# wrong. A group- or world-writable level lets another local user replace the staging
# directory or the published pointer after the check has passed.
for level in wtf-scope repo; do
  for mode in 777 770; do
    guard3="$WORK/guard-$level-$mode"
    mkdir -p "$guard3/wtf-scope"
    if [ "$level" = repo ]; then
      # Create the per-repo level by resolving once, then loosen that level only.
      inner3="$(cd "$WORK/republish" && TMPDIR="$guard3" "$RESOLVE" resolve --scope HEAD | tail -1)"
      chmod "$mode" "$(dirname "$inner3")"
    else
      chmod "$mode" "$guard3/wtf-scope"
    fi
    rc=0
    ( cd "$WORK/republish" && TMPDIR="$guard3" "$RESOLVE" resolve --scope HEAD ) >/dev/null 2>&1 || rc=$?
    check "a $mode $level level is refused" "1" "$rc"
  done
done


echo "== a jq failure is not mistaken for a subject =="
# Exit 2 is a contract, so nothing but the deliberate prose exit may produce it. jq
# itself exits 2 on a system error -- a missing --slurpfile input, a full TMPDIR -- and
# under `set -e` that status would have become the script's.
#
# Driven through the real script rather than a copy of the trap: a hand-copied guard
# passes this case whether or not resolve-scope.sh still carries one. The stub sits
# ahead of the real jq on PATH and still satisfies the `command -v jq` preflight, then
# fails the first time the script actually asks it to do something.
stub="$WORK/jq-stub"
mkdir -p "$stub"
cat > "$stub/jq" <<'JQ'
#!/bin/sh
exit 2
JQ
chmod +x "$stub/jq"

rc=0; ( PATH="$stub:$PATH" jq --version ) >/dev/null 2>&1 || rc=$?
check "the stub really exits 2, so the guard is load-bearing" "2" "$rc"

rc=0
( cd "$WORK/republish" && PATH="$stub:$PATH" "$RESOLVE" resolve --scope HEAD ) >/dev/null 2>&1 || rc=$?
check "a jq system error leaves the script at 1, not 2" "1" "$rc"

if [ "$failures" -gt 0 ]; then
  echo "$failures check(s) failed" >&2
  exit 1
fi
echo "all checks passed"
