---
name: wtf-verify-fix
description: 'Prove a change does what it claims before the PR goes out, by running one probe against the merge-base (must fail) and against HEAD (must pass) and reporting both with raw evidence. RUN ONLY when a human explicitly asks, or at the single moment just before a PR is opened for a change that claims a fix — it builds and bootstraps a second worktree and can take many minutes. Triggers on "verify the fix", "e2e test this change", "spin up an environment and check it works", "prove the bug was real", "confirm this actually works end to end", or an explicit ask to check a branch before opening the PR. Once per branch, not per commit. Works in any language (Node, Rust, Elixir, Erlang, Elisp) and also covers refactors (output equivalence) and perf work (measured threshold). NOT for: running or writing tests normally, code review, linting, CI triage, or checking an unfinished change mid-development.'
argument-hint: '[--base <ref>] [--tier 0|1|2|3]'
allowed-tools: Bash, Read, Edit, Write, Glob, Grep
---

# Verify the fix

A test that passes on your branch proves nothing on its own. It might have passed
before the change too. The claim in a PR body is always a **difference** — "it was
broken, now it isn't" — so the verification has to be a difference as well: the same
probe, run against the base and against HEAD, producing two different results.

That is the whole method. Everything below exists to stop the four ways it quietly
turns into theatre: a probe that never measured the bug, a baseline that fails for
environmental reasons, shared state that carries an answer between the two runs, and
a model that rationalizes whatever it got instead of what it predicted.

Read `references/environments.md` when you need to work out how to run the thing in
this particular project. Read `references/evidence.md` when you are ready to report.

## 0. Check this is the right moment

This is an expensive, end-of-cycle gate, not a background check. It creates a second
worktree, installs and builds it, and at the upper tiers boots services — minutes at
best, longer in a large repo. That cost only makes sense once the change is finished
and about to be defended in a PR.

Run it when a human asks for it, or once on a finished branch immediately before the
PR. Do not run it after ordinary edits, on every commit, as a substitute for the test
suite, or as an unrequested step inside some larger workflow. If a change is still
moving, the verdict will be stale before anyone reads it.

Two things to do before starting:

- Say which tier you intend to use and roughly what it will cost, then start. If the
  probe needs tier 2 or 3 — booting services, driving a browser — ask first rather
  than assuming the user wants to spend that.
- Check the change is committed. The baseline is `git merge-base HEAD <base>`, so
  uncommitted work is invisible to the comparison and the script will refuse.

## 1. State the claim as an observable

Read the diff and the ticket, then write one sentence in this shape:

> Before this change, `<input>` produces `<wrong observable>`. After, it produces `<right observable>`.

Both observables must be things you could show someone who has never read the code —
a response body, a rendered file, a log line, an exit code, a pixel. "The cache is no
longer invalidated twice" is not observable. "The second request returns a 200 with
the updated title instead of the stale one" is.

If you cannot write that sentence, stop and say so. Either the change is not a fix
(go to §9) or you do not yet understand what it does, and spinning up an environment
will not tell you.

## 2. Design the probe

One probe. One observable. Write down the exact expected before-string and
after-string **before you run anything** — predicting the failure is what stops you
from accepting whatever the baseline happens to emit as confirmation.

Four rules, in priority order:

**It must run on both sides.** This is the trap that produces the most confident
wrong answers. If the probe imports a symbol the change introduces, passes a flag the
change adds, or reads a config key that only exists on HEAD, then the baseline run
fails with `Cannot find module` or `undefined is not a function` — and that is not
the bug, it is your probe failing to compile. Exercise the change through a surface
both trees already have. When the change genuinely adds a new entry point and no
shared surface exists, say so explicitly in the report and treat the baseline
evidence as weaker.

**It must observe from outside.** Assertions about internal call counts and private
state drift with refactors and tend to be written to match the code you just read.
Prefer the outermost observable that still isolates the symptom.

**It must be deterministic.** Same input, same output, run twice. If it isn't, you
cannot tell a fix from a coin flip. Run it twice on HEAD before you trust it.

**It must be narrow.** A probe that boots the whole product and eyeballs a page will
find *something* different between two commits and invite you to call that the bug.

## 3. Pick the cheapest tier that can see it

Escalate only when the tier below is genuinely blind to the symptom. Each step up
costs minutes and adds ways for the environment, rather than the code, to decide the
answer.

| Tier | Environment | Cost | Use when the symptom lives in |
| ---- | ----------- | ---- | ----------------------------- |
| 0 | a test in one package or module | seconds | logic, contracts, transforms, parsers |
| 1 | headless script, CLI, or a real build/render | ~1 min | pipelines, cross-package wiring, generated output |
| 2 | the service booted, driven over its real interface | minutes | API shape, persistence, auth, background jobs |
| 3 | full stack and a browser | many minutes | UI, hydration, anything visual |

Most changes are tier 0 or 1. `references/environments.md` covers how to find the
right command in a project you do not have memorized, and the hazards specific to
each tier.

## 4. If it can be a test, make it one

When a tier-0 probe can express the bug, write it as a real test in the project and
commit it. It costs nothing extra — the same file serves as the probe for the
differential run — and it converts a one-off verification into a permanent guard. A
bug that was worth proving is worth catching again.

The differential still applies: the new test must fail in the baseline worktree and
pass on HEAD. A new test that passes on both is the clearest possible signal that it
does not test the bug.

Write it as a test unless the symptom needs a live environment. If you decide it
cannot be, say which tier-0 boundary it crosses.

## 5. Build the baseline

```bash
~/.claude/skills/wtf-verify-fix/scripts/baseline-worktree.sh create --base main
```

This creates a worktree at `git merge-base HEAD <base>` **outside the repo** (under
`$TMPDIR`), detects which ecosystems the project uses, and bootstraps each one:
dependencies fetched, `.env` symlinked, sources compiled. It knows Node, Rust,
Elixir, Erlang and Elisp; `detect` prints what it found, and a repo can be several at
once. Useful flags:

- `--head <ref>` name the "after" side explicitly instead of using your checkout
- `--ecosystem <name>` force one when detection is wrong or the repo is mixed
- `--filter <pkg>` scope the build to one package or crate
- `--copy <relpath>` copy a gitignored generated artifact in (repeatable)
- `--install-cmd '<cmd>'` / `--build-cmd '<cmd>'` replace detection entirely — the
  right answer for any toolchain it does not know
- `--no-build` when nothing needs compiling
- `remove` to tear both down, `path [baseline|head]` to print a location

The merge base, not the tip of `main`: you are isolating **your** change, and
anything that landed on `main` since you branched is somebody else's variable.

By default the "after" side is your working checkout. `--head <ref>` names it instead
and builds a second worktree for it — which you need when the change is already
merged, or sits on a branch you would rather not check out. It also makes the two
sides symmetric: both freshly bootstrapped, neither carrying whatever your working
checkout has accumulated. For a change that already landed, point `--head` at the
branch tip and `--base` at the commit it forked from; naming `--base main` there
compares the change against a branch that already contains it, and the script says so
and works out the fork point for you.

Bootstrapping is the step that goes wrong most often. A worktree shares git history
but not gitignored files, so a fresh one has no dependencies, no compiled output, no
`.env` and no generated code — `node_modules/` and `dist/`, `target/`, `_build/` and
`deps/`, `.elc` files, whichever applies. The resulting failures are
indistinguishable from a real reproduction, which is how a broken baseline gets
reported as a proven bug. The script ends by listing anything gitignored that the
main checkout has and the baseline does not; if a failure there names one of those
paths, that is a bootstrap gap, not your bug.

Outside the repo rather than nested inside it, because build tools infer the
workspace root by walking up from the working directory, and a worktree nested under
the checkout gives them two plausible roots to choose between.

## 6. Run the control before you trust the baseline

Before the baseline's failure means anything, prove the baseline can succeed at all.
Run something in it that must pass on both sides — the package's existing test file,
a neighbouring endpoint, the unchanged page next to the broken one.

A baseline that fails the control is a broken environment, not a demonstrated bug,
and the difference is invisible in the output you were hoping to see. This is the
single step most likely to be skipped and the one that most often changes the
verdict.

## 7. Run the differential

Run the probe in the baseline worktree, then on HEAD. Capture raw stdout and stderr
to files verbatim — the report quotes them, and a summarized error is exactly where a
compile failure gets laundered into "the bug reproduced."

For tiers 2 and 3 the two runs cannot overlap: they bind the same ports. Bring shared
dependencies up once, then run the app from one tree at a time, stopping between. And
treat the database as what it is — shared mutable state that both runs touch. If the
baseline run writes a row that makes the HEAD run pass, you measured data, not code.
Use a fresh entity per run, reset between, or run the pair in both orders and confirm
the result doesn't move.

## 8. Adjudicate honestly

| Baseline | HEAD | Verdict |
| -------- | ---- | ------- |
| fails with the predicted symptom | passes | **Verified.** Report both. |
| **passes** | passes | **Not verified.** Go back to §2. |
| fails for another reason (module resolution, missing env, port in use) | any | **Invalid probe or bootstrap.** Fix it and rerun; do not report this as a reproduction. |
| fails with the predicted symptom | **fails** | **The fix does not work.** Report it plainly. |

The second row is the one that matters. A baseline that passes means one of three
things, and you have to pick: the probe does not exercise the bug; the bug needs a
condition you did not reproduce; or the change is not a fix. None of them is
"verified," and none is a reason to loosen the probe until the baseline fails. Say
which you think it is and what you would need to tell them apart.

The fourth row is a success for this process, not a failure. Finding it now is the
entire point of running before the PR.

## 9. Changes that are not bugfixes

The differential still works, but the expected polarity changes. Forcing a bugfix
frame onto these produces nonsense.

**Refactors** claim *no* observable difference. So the probe is an equivalence check:
run the same input through both trees and diff the outputs. Expected result is both
sides passing with byte-identical output, and any difference is the finding. Pick an
input rich enough to cover the refactored paths — an equivalence proof over trivial
input proves the trivial case.

**Performance** claims a threshold, not a boolean. One run of each is noise. Take at
least five runs per side, report median and spread, and state the threshold before
measuring. Two distributions that overlap have not demonstrated anything, however
different the medians look. Say so rather than reporting the better median.

**Behind a flag**, run the probe with the flag both off and on. Off must match the
baseline exactly — that is the claim a flag makes.

## 10. Report

Give the terminal verdict first: verified or not, in one line, then the before/after
evidence. Write the PR verification section to the scratch directory alongside the
probe and the captured output, using the template in `references/evidence.md`, and
tell the user the path.

Say what you did not cover. A probe is narrow by design, and the honest report names
the part of the change that ran unexercised — the error branch you could not trigger,
the browser you did not open, the production data shape you approximated. That
sentence is worth more to a reviewer than another passing assertion.

Tear the baseline down when you are done:

```bash
~/.claude/skills/wtf-verify-fix/scripts/baseline-worktree.sh remove
```
