---
name: wtf-code-verify
description: 'Prove that code does what it is supposed to do before it merges, by executing it — derive the falsifiable expectations a change or a subject implies, probe each one including the inputs that must be refused, and report a verdict backed by raw output. Use this whenever a PR is about to be opened or changes are about to merge, and whenever the user asks to verify, prove, confirm or sanity-check that something actually works: "does this actually work", "verify the fix", "e2e test this change", "spin up the server and check it", "make sure auth still works before I merge". Takes a ref, branch, PR or a prose subject — "login and authentication" — so it also covers an area of a running system with no diff, and whether documentation or comments still match the implementation. Expensive by design — it boots services and builds a second worktree. NOT for reading code without running it (that is wtf-code-review), routine test runs, or checking unfinished work mid-development.'
argument-hint: '[ref, branch, PR, path or subject — defaults to uncommitted, else the branch] [--tier 0|1|2|3] [--base <ref>]'
allowed-tools: Agent, Bash, Read, Edit, Write, Glob, Grep
---

# Verify it actually works

A green check proves nothing on its own. It might have been green before the change. It
might be green against code that does not work. It might be measuring your dirty
checkout rather than the diff. Verification is not "run something and watch it pass" —
it is **discrimination**: a green result earns its meaning only next to a red one you
predicted in advance.

That is the idea the whole skill turns on. Everything below exists to stop the four
ways verification decays into theatre: expectations invented after the fact to fit
whatever happened, probes that would have passed anyway, environments that decide the
answer instead of the code, and a report that rounds an inconclusive run up to a pass.

**This skill runs code. `wtf-code-review` reads it.** If the scope has nothing that can
be executed and no claim that can be checked against a running system, say so and stop.
Do not slide into reading the code and presenting that as verification — it is a
different tool, it is one message away, and a reader cannot tell the two reports apart
once they are written.

Read each reference only when a claim calls for it:

| Reference | Read it when |
| --------- | ------------ |
| `references/expectations.md` | working out what "correct" means — per claim type, plus the catalogue of cases that should fail |
| `references/differential.md` | the claim *is* a difference: a bugfix, a refactor's equivalence, a perf threshold |
| `references/environments.md` | working out how to run this particular project, at any tier, and how to isolate it |
| `references/evidence.md` | capturing output and writing the report |
| `references/promotion.md` | turning a probe into a permanent test |

## 0. Check this is the right moment

This is an end-of-cycle gate, not a background check. At the upper tiers it boots
services and builds a second worktree — minutes at best, longer in a large repo. That
cost only makes sense once the change is finished and about to be defended in a PR.

Run it when a human asks, or once on a finished branch immediately before the PR. Not
after ordinary edits, not on every commit, not as a substitute for the test suite, and
not as an unrequested step inside some larger workflow. If the change is still moving,
the verdict is stale before anyone reads it.

Before starting: say which tiers you expect to need and roughly what that costs, then
begin. If a claim needs tier 2 or 3 — booting services, driving a browser — ask first
rather than assuming the user wants to spend that. And commit the change before any
claim that will use a differential: uncommitted edits are live on the head side and
absent from the baseline, so they get credited to the change and never reach the PR.

## 1. Establish the scope

A named scope comes in three shapes.

**A revision** — a ref, a branch, a path, a PR number. Diff it. `gh pr diff <n>` and
`gh pr view <n>` for a PR, which also gives you the body, and the body is where the
author already wrote down what they think they did.

**A subject** — prose naming an area of behaviour, such as `login and authentication`.
There is no diff and no author here: you are verifying a contract as it stands. Find
what implements the subject, name the files you settled on and say how you found them.
A subject scope is the one case where *you* choose what gets verified, and a reader who
cannot see that choice has no way to tell whether the run covered the thing they meant.
If nothing in the repo plausibly implements the subject, say so and stop rather than
verifying the nearest thing you found.

**Nothing** — resolve it yourself, in this order: uncommitted work
(`git status --porcelain`, `git diff`, `git diff --staged`), else the branch against its
merge base (`git diff $(git merge-base HEAD main)...HEAD`), else `git show HEAD`.

State the scope you settled on before you run anything.

## 2. Classify each claim, not the change

"Verified" means different things depending on what is being claimed, and forcing one
frame onto all of them produces nonsense. A single PR usually makes several claims of
different kinds — classify each one.

| The claim | Verified means | Read |
| --------- | -------------- | ---- |
| a bug is fixed | the symptom is present on the base and absent on HEAD | `references/differential.md` |
| a capability is new | it works, its unhappy paths fail correctly, and what worked before still works | `references/expectations.md` |
| nothing observable changed (refactor) | the same input produces byte-identical output on both trees | `references/differential.md` |
| it is faster | a threshold stated in advance, met across distributions rather than a single pair of numbers | `references/differential.md` |
| the prose is accurate | each falsifiable claim in it holds against the implementation, which is the source of truth | `references/expectations.md` |
| a subject behaves (no diff) | the contract holds, including its refusals | `references/expectations.md` |

## 3. Write the expectations down, then show them

Before running anything, write the list. Each line pairs an input or action with an
observable — something you could show to someone who has never read the code: a status
code, a response field, an exit code, a rendered file, a log line, a pixel. "The cache
is no longer invalidated twice" is not an observable. "The second request returns 200
with the updated title instead of the stale one" is.

Three kinds, and the second is where the bugs actually are:

- **positive** — the thing works. The part that was always going to pass.
- **negative** — the inputs that must be *refused*, and refused correctly. A 500 where a
  400 belongs, a bad parameter silently defaulting instead of erroring, an auth check
  that fails open, a stack trace in the response body. `references/expectations.md`
  carries the catalogue; the point is that the expectation names *how* it fails, not just that it does.
- **regression** — what worked before still works. The change's blast radius.

Cover a claim per meaningful area of the change rather than one probe for the whole
thing: a five-file change with one probe leaves four areas unexercised, and the report
reads as though it covered them. Pick the cheapest tier that can see each claim
(§5) so breadth stays affordable. If the list runs past what the budget allows, say
which claims you dropped and why — silently probing two of six is the failure this
whole section is here to prevent.

```
| # | Kind | Input | Expected observable |
| - | ---- | ----- | ------------------- |
| 1 | positive   | `posts(includeArchived: true)` as an editor | 200; `data.posts` contains archived id 7 |
| 2 | negative   | same query, anonymous caller | 401 `UNAUTHENTICATED`; `data.posts` absent |
| 3 | negative   | `includeArchived: "banana"` | 400 with a field-level type error — not a 500, not a silent default to false |
| 4 | regression | `posts` with no new argument | unchanged: archived rows absent |
```

Then **show the list to the user before running anything.** Their knowledge of the
domain is exactly what fills the gap you cannot see — "you forgot that the token can be
expired rather than missing" — and one message is far cheaper than a wasted tier-2 run
built on the wrong idea of correct.

**Predict each result before executing it.** Write the expected string down. Afterwards
everything looks like confirmation: a 500 reads as "rejected", an empty array reads as
"filtered correctly", a timeout reads as "slow but working". A prediction on the page is
the only thing that makes those wrong later.

### The adversary pass

For the negative cases, dispatch the `wtf-verify-adversary` subagent with the Agent tool
and give it **only** the diff or the subject — none of your reasoning, and none of the
expectations you already wrote. Whoever wrote the code has already imagined the inputs
it handles; the value is entirely in the ones they did not, and a fresh reader is the
one positioned to name those.

Worth the dispatch when the change has a surface that takes input from outside: an API,
a CLI, a form, a parser, a queue consumer, anything with an auth boundary. Skip it for a
refactor or a prose change, where "inputs that should fail" is not the interesting
category. It proposes cases and expected refusals; it does not run anything. Merge what
it returns into your list, dropping any case whose expected refusal you cannot state
concretely.

## 4. Give every probe a discriminating partner

A probe you have only ever seen green is not evidence. Three ways to earn discrimination,
cheapest first — pick one per claim:

1. **The negative case is its own control.** If the invalid input produces the same
   result as the valid one, the code is not reading it. Free: you already wrote both.
2. **Break it deliberately.** Corrupt the input, drop the header, flip the flag off,
   point at an id that does not exist. Confirm the probe goes red, and red for the reason
   you expect. Seconds, no worktree.
3. **Run it against the base.** The full differential, and the only form that shows *the
   change* caused the difference. Costs a worktree and a bootstrap —
   `references/differential.md`.

A bugfix claim needs (3): the claim is itself a difference, so nothing weaker can carry
it. A new capability usually takes (1) plus (2), because on the base the surface does not
exist — "the base rejects an argument it has never heard of" is true and tells you
nothing about whether HEAD does the right thing. Prose claims get neither: what
discriminates there is checking the claim against the implementation rather than against
your memory of it.

The trap that produces the most confident wrong answers: **a probe that cannot run on
both sides.** If it imports a symbol the change adds, passes a flag the change adds, or
reads a config key that only exists on HEAD, the baseline fails with `Cannot find module`
— and that is your probe failing to compile, not the bug reproducing.

**Run the head probe twice.** Non-determinism is indistinguishable from a fix. A probe
that passes and then fails on re-run has found something real — leftover state, a unique
constraint, a cached response — and that is a finding, not a flake to retry away.

## 5. Pick the cheapest tier, and the isolation to match

Escalate only when the tier below is genuinely blind to the symptom. Each step up costs
minutes and adds ways for the environment, rather than the code, to decide the answer.

| Tier | Environment | Cost | Use when the claim lives in |
| ---- | ----------- | ---- | --------------------------- |
| 0 | a test in one package or module | seconds | logic, contracts, transforms, parsers |
| 1 | headless script, CLI, or a real build/render | ~1 min | pipelines, cross-package wiring, generated output |
| 2 | the service booted, driven over its real interface | minutes | API shape, persistence, auth, background jobs |
| 3 | full stack and a browser | many minutes | UI, hydration, anything visual |

`--tier N` pins the tier when the user has already decided what to spend; `--base <ref>`
names the comparison point for any claim that uses a differential.

Isolation buys two specific things: **attributability**, so a failure belongs to the code
rather than to your checkout, and **containment**, so the probe leaves nothing behind. It
is not a virtue in itself, and isolation you did not need is time you did not have.

| You need | Use | Because |
| -------- | --- | ------- |
| to run two versions of the tree | a second worktree — `~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh` | a fresh worktree has none of the gitignored state that makes your checkout unreproducible |
| a probe that writes state | the project's own compose file, test database or fixtures | shared mutable state between runs measures your data, not your code |
| only HEAD, at tier 0–1 | your checkout | nothing to isolate from |

**Never point a probe at a shared environment** — not staging, not production, not a
colleague's instance. And before any probe that can send something outward (email, SMS,
webhook, push notification, payment, a write to a third-party API) find the project's
sandbox or mock mode and confirm you are in it. This is the only part of this skill
capable of damage that outlives the session, and an accidental send cannot be undone by
tearing down a worktree.

Do not build a container the project does not already have. Use its compose file,
devcontainer or Makefile target if one exists; if none does, a hand-rolled image means
spending the session debugging your Dockerfile and learning nothing about the change.
Drop a tier instead. `references/environments.md` has the detail per tier, per language,
and per isolation mechanism.

**Check what CI already runs** — `.github/workflows`, `.gitlab-ci.yml`, whatever the repo
uses — before designing probes. A probe that duplicates a job which runs on every push
has spent minutes to tell you what a green check already said. The probes worth building
are the ones CI does *not* run, and the report should say so plainly: "CI covers the unit
suite and a smoke test on every push; this probed the archived-post authorisation path,
which it does not."

## 6. Run

Capture raw stdout, stderr and the exit code to files, verbatim, per `evidence.md`.
Summarizing as you capture is how a `Cannot find module` in the baseline gets written
down as "the expected failure". Write the bytes first, read them second.

Serialize anything that binds a port or touches a database — two trees want the same
port, and a row written by one run can be what makes the next one pass. Keep shared
dependencies (database, cache, queue) up across runs, since they are state rather than
code, and use a fresh entity per run, or reset between, or run the pair in both orders
and confirm the verdict does not move.

## 7. Adjudicate

| Verdict | When |
| ------- | ---- |
| **Verified** | every expectation met, each with a discriminating partner that behaved as predicted |
| **Verified with gaps** | what you ran passed, but part of the change or subject went unexercised — name the part |
| **Not verified** | the probe could not discriminate: green on the base too, red for environmental reasons, or non-deterministic. Say which of the three, and what would tell them apart |
| **Falsified** | an expectation failed for a real reason. A defect, found before the merge |

`Not verified` is not a pass and not a defect, and the pull towards recording it as one
or the other is strong. A probe that was green on both sides means the probe does not
exercise the claim, or the claim needed a condition you did not reproduce, or the change
does not do what it says. Pick one and say so — and never loosen a probe until it
finally goes red, which converts an honest inconclusive into a fabricated pass.

**Falsified is a success for this process.** Finding it now is the entire point of
running before the PR. Report the failure with its reproduction first, then offer to fix
it and re-verify — ask; do not assume. If you do fix and re-run, the report says the
verdict describes post-fix code and quotes the original failure verbatim. A report
showing only the final green run has erased the most valuable thing the run produced.

## 8. Report

Lead with the verdict in one line. A "not verified" that arrives after three paragraphs
of process gets read as a success.

Then the evidence, then three lines that are worth more to a reviewer than another
passing assertion:

- **Coverage** — which parts of the change or subject a probe actually exercised, and
  which it did not. Every probe is narrow; naming the gap tells a reviewer where to look,
  and its absence invites them to assume there isn't one.
- **CI overlap** — what already runs on every push, so this run's contribution is legible.
- **Residue** — rows, files, containers, worktrees, ports left behind, or explicitly
  nothing. Tear the worktrees down:
  `~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh remove`.

Write the PR verification section to the scratch directory with the template in
`evidence.md`, tell the user the path, and offer to post it — appended to the PR body or
as a comment via `gh`. Ask before posting: it is public and outward-facing, and a
verification section landing on the wrong PR is worse than none at all.

## 9. Offer to promote the probes

A probe worth writing is often worth keeping, but not always — and offering to promote
all of them is how a suite gets slow, flaky and eventually ignored. Triage, then ask.

Promote a probe when it is deterministic, runs inside the project's existing harness,
costs little enough that nobody will be tempted to skip it, and would catch this
regression again. Leave it throwaway when it boots the whole stack to observe one field,
depends on data you hand-made, asserts on wording that will legitimately change, or
would need a test harness the project does not have — adding that harness is a larger
change than the one under review, and belongs in its own PR.

Present it as a numbered list with a recommendation and a reason per probe, then ask.
Write nothing until they say yes.

```
Probes run:
  1. includeArchived=true returns the archived row     → promote: deterministic, 0.3s, fits tests/api/posts_test
  2. includeArchived="banana" returns a 400            → promote: same file, covers the negative contract
  3. Browser check of the archive toggle               → keep throwaway: 3 min, needs a seeded DB, and 1+2 already pin the contract

Promote 1 and 2?
```

On a yes: rewrite each into the project's own idiom — its framework, fixtures, naming
and file layout — rather than dropping a probe script into `tests/`. Read a neighbouring
test first and match it. And the discriminating rule applies to the promoted test too:
it must fail against the code that lacked the fix. The baseline worktree is probably
still up, so this costs one run. A promoted test that passes on both sides asserts
nothing, and it will be trusted for years. `references/promotion.md` has the detail,
including the substitute check for a capability the base cannot exercise at all.
