## command line tools available

These are installed on this machine. Use them rather than asking whether they
are available or hand-rolling a substitute:

- `magick` — image conversion, resizing and inspection. ImageMagick v7, so drive
  it as `magick in.png out.jpg`. IMv7 still ships a `convert`, but warns that it
  is deprecated; prefer `magick`.
- `ast-grep` — structural search over a tree-sitter AST. Use for questions grep
  is bad at, such as "does this shape already exist elsewhere?"
- `shellcheck` — lint any shell script before proposing it.
- `pandoc` — document format conversion.
- `docker` — container work; the CLI comes from Docker Desktop.


## Flags typed on iOS arrive with the wrong dash

I work from the Claude iOS app as well as the terminal, and its smart
punctuation rewrites `--` to an em dash as it is typed. So a flag reaches you as
`—lite` or `—draft`, sometimes with an en dash instead.

**Read a leading `—`, `–` or `−` on an argument as `--`.** This holds for every
command and skill, not just the ones whose own text mentions it.

It matters because the failure is silent rather than loud. A command splits its
arguments into the flags it knows and free text — a scope, a subject, body
context — and an unrecognised flag is not an error, it lands in the free text.
So `—lite` becomes a path to review that does not exist, and `—draft` becomes
the word "draft" in a pull request body. Both look like the user asked for
something odd rather than like a flag that went unread.

Canonical spelling stays `--lite`. This widens only what is *read* as a flag;
write it with two hyphens when you echo it back.


## Before You Start

Read the relevant reference docs in `~/.claude/reference/`:

| File                       | When to Read                             |
|----------------------------|------------------------------------------|
| `bug-investigation.md`     | Debugging issues that resist quick fixes |
| `code-review-checklist.md` | Reviewing code or preparing a PR         |


## Behavioral guidelines

Behavioral guidelines to reduce common LLM coding mistakes. Merge with project-specific instructions as needed.

**Tradeoff:** These guidelines bias toward caution over speed. For trivial tasks, use judgment.

## 1. Think Before Coding

**Don't assume. Don't hide confusion. Surface tradeoffs.**

Before implementing:
- State your assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them - don't pick silently.
- If a simpler approach exists, say so. Push back when warranted.
- If something is unclear, stop. Name what's confusing. Ask.

## 2. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code.
- No "flexibility" or "configurability" that wasn't requested.
- No error handling for impossible scenarios.
- If you write 200 lines and it could be 50, rewrite it.

Ask yourself: "Would a senior engineer say this is overcomplicated?" If yes, simplify.

## 3. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

When editing existing code:
- Don't "improve" adjacent code, comments, or formatting.
- Don't refactor things that aren't broken.
- Match existing style, even if you'd do it differently.
- If you notice unrelated dead code, mention it - don't delete it.

When your changes create orphans:
- Remove imports/variables/functions that YOUR changes made unused.
- Don't remove pre-existing dead code unless asked.

The test: Every changed line should trace directly to the user's request.

## 4. Goal-Driven Execution

**Define success criteria. Loop until verified.**

Transform tasks into verifiable goals:
- "Add validation" → "Write tests for invalid inputs, then make them pass"
- "Fix the bug" → "Write a test that reproduces it, then make it pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
3. [Step] → verify: [check]
```

Strong success criteria let you loop independently. Weak criteria ("make it work") require constant clarification.

## 5. Comments Earn Their Place

**Explain the WHY, only where it isn't already obvious. Default to no comment.**

The common failure is not the obviously useless comment — it is the *true,
well-reasoned* one that costs more to read than the code it sits above.

- Never restate the code, the type, or the symbol name. `// Props for the
  scale diagram` above `ScaleDiagramProps` is pure noise.
- Length must fit the code, not the thinking that produced it. A one-line
  constant does not get a five-line justification, however accurate. If the
  reason needs a paragraph, the paragraph belongs in the PR description or
  commit message; the code keeps the one line a reader needs at that spot.
- State a shared reason once. If several adjacent declarations turn on the same
  invariant, put one comment above the group — not the same rationale reworded
  on each. Repeating it teaches readers to skim past the block entirely.
- Don't restate above what you already said below (or vice versa) in the same
  file.
- Never write a claim you have not checked against the implementation. A stale
  or wrong comment is worse than none: it outlives the code and misleads.
- **Do** comment regex and bitwise expressions — one line on what it matches or
  does. These are the cases a human genuinely cannot read at a glance.

```ts
// ❌ true, and still too much for what it guards
// SendGrid can redeliver the same event; a crash between commit and the
// job-complete ack can also redeliver it. Either way the unique constraint
// on (provider_type, prov_event_id) rolls the whole transaction back, and
// we treat that as a successful no-op rather than a failure.
if (isUniqueViolation(error)) {

// ✅ same point, one line
// Ingestion is idempotent, so a duplicate is a no-op rather than a failure.
if (isUniqueViolation(error)) {
```

---

**These guidelines are working if:** fewer unnecessary changes in diffs, fewer rewrites due to overcomplication, and clarifying questions come before implementation rather than after mistakes.
