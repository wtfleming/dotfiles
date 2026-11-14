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

tree_of_worktree() {
  local index real
  index="$(mktemp)"
  # Seeded from the real index so unchanged files are not rehashed.
  real="$(git rev-parse --git-path index)"
  if [ -f "$real" ]; then cp "$real" "$index"; else rm -f "$index"; fi
  GIT_INDEX_FILE="$index" git add -A
  GIT_INDEX_FILE="$index" git write-tree
  rm -f "$index"
}

top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git working tree"
cd "$top"

case "${1:-}" in
  take)
    tree_of_worktree
    ;;
  diff)
    before="${2:?diff needs the tree id that take printed}"
    git rev-parse --verify --quiet "$before^{tree}" >/dev/null || die "not a tree: $before"
    after="$(tree_of_worktree)"
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
