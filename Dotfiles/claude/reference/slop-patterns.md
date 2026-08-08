# AI Slop Patterns

What machine-written code does that human-written code in this repo does not.
Two consumers read this: `wtf-change-reviewer` flags these as Suggestions, and
the `wtf-deslop` skill removes them. The catalogue describes the patterns;
what to do about them belongs to whoever is reading.

The universal test: **does this line match how the rest of the file is
written?** The file's own idiom wins over anything below. Examples span
TypeScript, Rust, Elixir and Emacs Lisp, which is most of what gets written
here.

## Excessive Comments

Comments that state the obvious, repeat the name of the thing below them, or
explain *what* instead of *why*.

```typescript
// ❌ Check if the user is valid
if (isValidUser(user)) {
```
```elisp
;; ❌ Set the mode to fundamental
(fundamental-mode)
```
```rust
// ✅ Fine: explains why
// Sorted insert instead of push+sort: callers read between every insert.
```

Docstrings and doc comments follow the file: if the surrounding functions have
`///` / `@doc` / docstrings, new ones should too — and if they don't, new ones
are noise.

## Escape Hatches

Silencing the compiler or the error instead of satisfying it. Each language
has its own flavour:

- **TypeScript** — `as any`, `as unknown as T`, `!` non-null assertions,
  `@ts-ignore`. The honest form: narrow the type, add the interface, write the
  type guard.
- **Rust** — `.unwrap()`/`.expect()` sprinkled to make it compile, `.clone()`
  to appease the borrow checker, `#[allow(...)]` on warnings the code caused.
  The honest form: propagate with `?`, restructure the ownership, fix the
  warning.
- **Elixir** — catch-all `_ ->` clauses and `rescue _ ->` that swallow what
  should crash. The honest form: let it crash, or match the cases that can
  actually occur.
- **Elisp** — `ignore-errors` wrapped around calls that were not failing,
  `condition-case` with an empty handler.

An escape hatch the codebase already uses deliberately (a documented `unwrap`
on an invariant, a `!` after an explicit check) is idiom, not slop.

## Gratuitous Defensive Checks

Guarding against states that cannot occur: re-checking a value validated one
call up, null-checking a parameter every caller constructs, `is_nil`/`(when x
...)` around values that are never nil, `if let Some` on something just
matched. A check is only justified by a caller that can actually produce the
bad value.

## Verbose Logging

Narrating progress. `console.log("Processing started")`, `dbg!`/`println!`
left behind, `IO.inspect` on every pipeline stage, `(message "entering foo")`.
The file's existing logging level is the standard — error logging that follows
the established pattern is fine, narration is not. (Noisy logging while
actively debugging is fine; this is about what gets committed.)

## Style Inconsistencies

Anything that differs from the rest of the file: naming convention, import
style, error-handling shape, comment density. A diff should read as if the
file's original author kept typing.
