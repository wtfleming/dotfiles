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
5. **Only where the user named a PR** — this skill resolves no scope of its own, step 1
   compares against the main branch, and nothing here supplies a PR number, so there is
   no number to guess at. Where they did: read that PR's body
   (`gh pr view <that number> --json body`) for a `<!-- verify:start -->` section. This
   pass just changed the code that section attested to and wrote no new one, so it now
   reads as current for a tree that no longer exists — say it is stale and name
   `wtf-code-verify` as the refresh. Where the read fails rather than coming back without
   markers, say so instead: an absent marker and an unreadable body are different facts.
   `~/.claude/reference/github-publishing.md` has why no delimiter catches this, and names
   this skill among the tools the rule binds.

```
Removed 3 redundant nil checks in order.ex (callers pattern-match the struct).
Deleted 8 obvious comments and replaced 2 unwraps with ? propagation.
```
