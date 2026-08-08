---
name: wtf-deslop
description: Strip AI-written slop from a diff — excessive comments, gratuitous defensive checks, escape hatches that silence the compiler. Edits files in place. Use only when explicitly asked to clean up AI slop; for reviewing or assessing changes, use the wtf-change-reviewer agent instead.
---

# Deslop

Remove AI-generated patterns that don't match human-written code. This skill
**edits code**. A request to review, assess, or check a branch is not a request
to change it — that is the `wtf-change-reviewer` agent, which reports and
touches nothing. If it is unclear which was meant, ask before editing.

The patterns below are language-independent; the examples span TypeScript,
Rust, Elixir and Emacs Lisp, which is most of what gets written here. The test
in every case is the same: **does this line match how the rest of the file is
written?** The file's own idiom wins over anything below.

## Excessive Comments

AI over-comments in every language. Remove comments that state the obvious,
repeat the name of the thing below them, or explain *what* instead of *why*.

```typescript
// ❌ Check if the user is valid
if (isValidUser(user)) {
```
```elisp
;; ❌ Set the mode to fundamental
(fundamental-mode)
```
```rust
// ✅ Keep: explains why
// Sorted insert instead of push+sort: callers read between every insert.
```

Docstrings and doc comments follow the file: if the surrounding functions have
`///` / `@doc` / docstrings, new ones should too — and if they don't, don't add
them.

## Escape Hatches

AI silences the compiler or the error instead of satisfying it. Each language
has its own flavour:

- **TypeScript** — `as any`, `as unknown as T`, `!` non-null assertions, `@ts-ignore`.
  Fix the type: narrow it, add the interface, write the type guard.
- **Rust** — `.unwrap()`/`.expect()` sprinkled to make it compile, `.clone()` to
  appease the borrow checker, `#[allow(...)]` on warnings the code caused.
  Propagate with `?`, restructure the ownership, fix the warning.
- **Elixir** — catch-all `_ ->` clauses and `rescue _ ->` that swallow what
  should crash. Let it crash, or match the cases that can actually occur.
- **Elisp** — `ignore-errors` wrapped around calls that were not failing,
  `condition-case` with an empty handler.

An escape hatch the codebase already uses deliberately (a documented `unwrap`
on an invariant, a `!` after an explicit check) is idiom, not slop — leave it.

## Gratuitous Defensive Checks

AI guards against states that cannot occur: re-checking a value validated one
call up, null-checking a parameter every caller constructs, `is_nil`/`(when x
...)` around values that are never nil, `if let Some` on something just matched.
Remove the check unless you can name the caller that produces the bad value.

## Verbose Logging

AI narrates progress. `console.log("Processing started")`, `dbg!`/`println!`
left behind, `IO.inspect` on every pipeline stage, `(message "entering foo")`.
Match the file's logging level — keep error logging that follows the existing
pattern, delete the narration. (Add noisy logging freely while debugging; this
is about what gets committed.)

## Style Inconsistencies

Anything that differs from the rest of the file: naming convention, import
style, error-handling shape, comment density. The diff should read as if the
file's original author kept typing.

## Process

1. Get the diff, compare against the main branch.
2. Scan each changed file for the patterns above, judging against the unchanged
   parts of the same file.
3. Make targeted fixes. Do not change correct code, and do not deslop lines the
   diff never touched.
4. Summarise in 1–3 sentences what was removed and why.

```
Removed 3 redundant nil checks in order.ex (callers pattern-match the struct).
Deleted 8 obvious comments and replaced 2 unwraps with ? propagation.
```
