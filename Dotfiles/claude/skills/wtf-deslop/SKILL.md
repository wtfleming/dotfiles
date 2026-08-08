---
name: wtf-deslop
description: Strip AI-written slop from a diff — excessive comments, gratuitous defensive checks, escape hatches that silence the compiler. Edits files in place. Use only when explicitly asked to clean up AI slop; for reviewing or assessing changes, use the wtf-change-reviewer agent instead.
---

# Deslop

Remove AI-generated patterns that don't match human-written code. This skill
**edits code**. A request to review, assess, or check a branch is not a request
to change it — that is the `wtf-change-reviewer` agent, which reports and
touches nothing. If it is unclear which was meant, ask before editing.

## What counts as slop

Read `~/.claude/reference/slop-patterns.md` — it is the single definition,
shared with the reviewer so the two never disagree about what slop is. The
short version: excessive comments, escape hatches that silence the compiler,
gratuitous defensive checks, narration logging, style drift. The file's own
idiom wins over the catalogue in every case.

## Process

1. Get the diff, compare against the main branch.
2. Scan each changed file for the catalogue's patterns, judging against the
   unchanged parts of the same file.
3. Make targeted fixes. Do not change correct code, and do not deslop lines the
   diff never touched.
4. Summarise in 1–3 sentences what was removed and why.

```
Removed 3 redundant nil checks in order.ex (callers pattern-match the struct).
Deleted 8 obvious comments and replaced 2 unwraps with ? propagation.
```
