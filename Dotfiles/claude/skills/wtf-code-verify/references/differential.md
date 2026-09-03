# The differential

The strongest form of discrimination: run the same probe against the base and against
HEAD, and show two different results. Use it when the claim *is* a difference — a bugfix,
a refactor's promise that nothing changed, a performance threshold. For a new capability
the cheaper partners in SKILL.md §4 usually serve better, because a surface the base has
never heard of cannot tell you anything about whether HEAD gets it right.

## Contents

- State the claim as a before/after
- Probe design — the four rules
- Build the baseline
- Run the control before you trust the baseline
- Run the differential
- Adjudicate
- Variants: refactor, performance, flagged
- A probe promoted to a test

## State the claim as a before/after

Read the diff and the ticket, then write one sentence in this shape:

> Before this change, `<input>` produces `<wrong observable>`. After, it produces `<right observable>`.

Both observables must be things you could show someone who has never read the code. If
you cannot write that sentence, stop: either the change is not a difference of this kind
— see the variants below — or you do not yet understand what it does, and spinning up an
environment will not tell you.

## Probe design — the four rules

One probe, one observable, and the expected before-string and after-string written down
**before you run anything**. Predicting the failure is what stops you from accepting
whatever the baseline happens to emit as confirmation.

**It must run on both sides.** This is the trap that produces the most confident wrong
answers. If the probe imports a symbol the change introduces, passes a flag the change
adds, or reads a config key that only exists on HEAD, then the baseline run fails with
`Cannot find module` or `undefined is not a function` — and that is not the bug, it is
your probe failing to compile. Exercise the change through a surface both trees already
have. When the change genuinely adds a new entry point and no shared surface exists, say
so explicitly in the report and treat the baseline evidence as weaker.

**It must observe from outside.** Assertions about internal call counts and private state
drift with refactors and tend to be written to match the code you just read. Prefer the
outermost observable that still isolates the symptom.

**It must be deterministic.** Same input, same output, run twice. If it isn't, you cannot
tell a fix from a coin flip. Run it twice on HEAD before you trust it.

**It must be narrow.** A probe that boots the whole product and eyeballs a page will find
*something* different between two commits and invite you to call that the bug.

## Build the baseline

```bash
~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh create --base main
```

This creates a worktree at `git merge-base HEAD <base>` **outside the repo** (under
`$TMPDIR`), detects which ecosystems the project uses, and bootstraps each one:
dependencies fetched, `.env` symlinked, sources compiled. It knows Node, Rust, Elixir,
Erlang and Elisp; `detect` prints what it found, and a repo can be several at once.
Useful flags:

- `--head <ref>` name the "after" side explicitly instead of using your checkout
- `--ecosystem <name>` force one when detection is wrong or the repo is mixed
- `--filter <pkg>` scope the build to one package or crate
- `--copy <relpath>` copy a gitignored generated artifact in (repeatable)
- `--install-cmd '<cmd>'` / `--build-cmd '<cmd>'` replace detection entirely — the right
  answer for any toolchain it does not know
- `--no-build` when nothing needs compiling
- `remove` to tear both down, `path [baseline|head]` to print a location

`create` refuses a dirty tree, and for good reason: by default the "after" side is your
working checkout, so uncommitted edits are live in the head run and absent from the
baseline. They would be credited to the change and never reach the PR. Stash them, or
name the change with `--head <ref>` instead.

The merge base, not the tip of `main`: you are isolating **your** change, and anything
that landed on `main` since you branched is somebody else's variable.

By default the "after" side is your working checkout. `--head <ref>` names it instead and
builds a second worktree for it — which you need when the change is already merged, or
sits on a branch you would rather not check out. It also makes the two sides symmetric:
both freshly bootstrapped, neither carrying whatever your working checkout has
accumulated. For a change that already landed, point `--head` at the branch tip and
`--base` at the commit it forked from; naming `--base main` there compares the change
against a branch that already contains it, and the script says so and works out the fork
point for you.

Bootstrapping is the step that goes wrong most often. A worktree shares git history but
not gitignored files, so a fresh one has no dependencies, no compiled output, no `.env`
and no generated code — `node_modules/` and `dist/`, `target/`, `_build/` and `deps/`,
`.elc` files, whichever applies. The resulting failures are indistinguishable from a real
reproduction, which is how a broken baseline gets reported as a proven bug. The script
ends by listing anything gitignored that the main checkout has and the baseline does not;
if a failure there names one of those paths, that is a bootstrap gap, not your bug.

Outside the repo rather than nested inside it, because build tools infer the workspace
root by walking up from the working directory, and a worktree nested under the checkout
gives them two plausible roots to choose between.

## Run the control before you trust the baseline

Before the baseline's failure means anything, prove the baseline can succeed at all. Run
something in it that must pass on both sides — the package's existing test file, a
neighbouring endpoint, the unchanged page next to the broken one.

A baseline that fails the control is a broken environment, not a demonstrated bug, and
the difference is invisible in the output you were hoping to see. This is the single step
most likely to be skipped and the one that most often changes the verdict.

## Run the differential

Run the probe in the baseline worktree, then on HEAD, with an **identical invocation on
both sides**. The two trees differ by one commit; if they also differ by how you ran the
probe, you have compared two procedures rather than two commits.

Capture raw stdout and stderr to files verbatim — the report quotes them, and a
summarized error is exactly where a compile failure gets laundered into "the bug
reproduced."

For tiers 2 and 3 the two runs cannot overlap: they bind the same ports. Bring shared
dependencies up once, then run the app from one tree at a time, stopping between. And
treat the database as what it is — shared mutable state that both runs touch. If the
baseline run writes a row that makes the HEAD run pass, you measured data, not code. Use
a fresh entity per run, reset between, or run the pair in both orders and confirm the
result doesn't move.

## Adjudicate

| Baseline | HEAD | Verdict |
| -------- | ---- | ------- |
| fails with the predicted symptom | passes | **Verified.** Report both. |
| **passes** | passes | **Not verified.** The probe does not exercise the claim, the claim needed a condition you did not reproduce, or the change does not do what it says. Pick one and say so. |
| fails for another reason (module resolution, missing env, port in use) | any | **Not verified** — invalid probe or bootstrap. Fix it and rerun; never report this as a reproduction. |
| fails with the predicted symptom | **fails** | **Falsified.** The fix does not work. Report it plainly; this is a success for the process. |

The second row is the one that matters, and the temptation there is to loosen the probe
until the baseline finally fails. That converts an honest inconclusive into a fabricated
pass, and it is the single worst outcome this method can produce.

## Variants

**Refactors** claim *no* observable difference. So the probe is an equivalence check: run
the same input through both trees and diff the outputs.

```bash
diff -r baseline-out/ head-out/
```

Expected result is both sides passing with byte-identical output, and any difference is
the finding. Pick an input rich enough to cover the refactored paths — an equivalence
proof over trivial input proves the trivial case, and the report should say what the
input covered.

**Performance** claims a threshold, not a boolean. One run of each is noise. Take at
least five runs per side, report median and spread, and state the threshold before
measuring. Two distributions that overlap have not demonstrated anything, however
different the medians look. Say so rather than reporting the better median.

**Behind a flag**, run the probe with the flag both off and on. Off must match the
baseline exactly — that is the claim a flag makes.

## A probe promoted to a test

When a tier-0 probe expresses the claim, the same file can serve as both the probe and
the permanent test — see `promotion.md`. The differential still applies, and it is what
makes the promoted test worth having: it must fail in the baseline worktree and pass on
HEAD. A new test that passes on both is the clearest possible signal that it does not
test the thing.

The baseline predates the test, so the file is not there — copy it in before running,
rather than committing it into the baseline tree, so the baseline stays the code as it
was. If the baseline run reports *no such test* or *no such file*, that is an invalid
probe, not a reproduction: the test never ran, so it measured nothing.
