#!/usr/bin/env bash
# Snapshot the working tree, and later diff the tree against that snapshot.
#
# /wtf-code-review's fix round needs a diff of the fixes alone, and once the edits have
# landed nothing separates them from the change they repair. A stash records no untracked
# files, and the reviewed change routinely includes some. So the snapshot is a tree object
# written from a throwaway index: it holds tracked and untracked files alike, honours
# .gitignore, and touches neither the real index nor the working tree.
#
# Usage:
#   fix-snapshot.sh take          print a tree id for the working tree as it stands
#   fix-snapshot.sh diff <tree>   print the path of a diff from <tree> to the working tree
#                                 as it stands now, or `empty` when nothing changed

set -Eeuo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

# Every step is checked by hand: this runs inside a command substitution, where bash
# clears errexit, and a failed `git add` would otherwise go on to write a tree that is
# missing the file it could not read.
tree_of_worktree() {
  local index real tree rc=0
  index="$(mktemp)" || return 1
  # Seeded from the real index so unchanged files are not rehashed.
  real="$(git rev-parse --git-path index)" || rc=$?
  if [ "$rc" -eq 0 ] && [ -f "$real" ]; then cp "$real" "$index" || rc=$?; else rm -f "$index"; fi
  [ "$rc" -ne 0 ] || GIT_INDEX_FILE="$index" git add -A || rc=$?
  [ "$rc" -ne 0 ] || tree="$(GIT_INDEX_FILE="$index" git write-tree)" || rc=$?
  rm -f "$index"
  [ "$rc" -eq 0 ] || return 1
  echo "$tree"
}

top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git working tree"
cd "$top"

case "${1:-}" in
  take)
    tree_of_worktree || die "could not snapshot the working tree"
    ;;
  diff)
    before="${2:?diff needs the tree id that take printed}"
    git rev-parse --verify --quiet "$before^{tree}" >/dev/null || die "not a tree: $before"
    after="$(tree_of_worktree)" || die "could not snapshot the working tree"
    if [ "$before" = "$after" ]; then
      echo empty
      exit 0
    fi
    umask 077
    out="$(mktemp -d)/fix.diff"
    git diff -M "$before" "$after" > "$out"
    echo "$out"
    ;;
  *) die "usage: $0 take | diff <tree>" ;;
esac
