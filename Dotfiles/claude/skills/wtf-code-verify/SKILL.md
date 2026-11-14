---
name: wtf-code-verify
description: 'Prove that code does what it is supposed to do before it merges, by executing it — derive the falsifiable expectations a change or a subject implies, probe each one including the inputs that must be refused, and report a verdict backed by raw output. Use only when the user asks to verify, prove, confirm or sanity-check that something actually works, including before opening a PR or merging: "does this actually work", "verify the fix", "e2e test this change", "spin up the server and check it", "make sure auth still works before I merge". Takes a ref, branch, PR or a prose subject — "login and authentication" — so it also covers an area of a running system with no diff, and whether the procedures and behaviour docs describe still hold. Expensive by design — it boots services and builds a second worktree. NOT an automatic pre-PR step nobody asked for, and NOT for reading code without running it (that is wtf-code-review), routine test runs, or checking unfinished work mid-development.'
argument-hint: '[ref, branch, PR, path or subject — defaults to uncommitted, else the branch] [--tier 0|1|2|3] [--base <ref>]'
allowed-tools: Agent, Bash, Read, Edit, Write, Glob, Grep
---

# Verify it actually works

A green check proves nothing on its own. It might have been green before the change. It
might be green against code that does not work. It might be measuring your dirty
checkout rather than the diff. Verification is not "run something and watch it pass" —
it is **discrimination**: a green result earns its meaning only next to a red one you
predicted in advance.

Everything below exists to stop the four ways verification decays into theatre:
expectations invented after the fact, probes that would have passed anyway, environments
that decide the answer instead of the code, and a report that rounds an inconclusive run
up to a pass.

**This skill runs code. `wtf-code-review` reads it.** If the scope has nothing that can
be executed and no claim that can be checked against a running system, say so and stop.
Do not slide into reading the code and presenting that as verification — a reader cannot
tell the two reports apart once they are written. Assume review has already run, so do
not re-report what a reader could have found; confirming a defect review only suspected
is worth doing, because that turns a suspicion into a fact.

Read each reference only when a claim calls for it:

| Reference | Read it when |
| --------- | ------------ |
| `references/expectations.md` | working out what "correct" means — per claim type, plus the catalogue of cases that should fail |
| `references/differential.md` | the claim *is* a difference: a bugfix, a refactor's equivalence, a perf threshold |
| `references/compatibility.md` | the change touches persisted state, a deploy boundary, or anything another system consumes |
| `references/environments.md` | working out how to run this particular project, at any tier, and how to isolate it |
| `references/evidence.md` | capturing output and writing the report |
| `references/promotion.md` | whether anything guards the change, and turning a probe into a permanent test |

## 0. Check this is the right moment

This is an end-of-cycle gate. At the upper tiers it boots services and builds a second
worktree, which only makes sense once the change is finished. Run it when a human asks,
or once on a finished branch immediately before the PR — not after ordinary edits, not
as a substitute for the test suite, and not as an unrequested step inside some larger
workflow. If the change is still moving, the verdict is stale before anyone reads it.
Under `wtf-ship` it is requested: invoking that skill is the ask, and its intake agreed the
tier in advance — its **The authority this run carries** names which pauses that replaces.

Commit the change before any claim that will use a differential: uncommitted edits are
live on the head side and absent from the baseline, so they get credited to the change
and never reach the PR.

## 1. Establish the scope

**A revision** — a ref, a branch, a path, a PR number. Resolve it with
`~/.claude/scripts/resolve-scope.sh resolve --scope <it>`. On a PR, `gh pr view <n>` gives
you the body, where the author already wrote down what they think they did: its
behavioural claims are expectations like any other. Whether the title and body still
describe the change is `/wtf-create-pr`'s check, not this one.

**A subject** — prose naming an area of behaviour, such as `login and authentication`.
There is no diff and no author: you are verifying a contract as it stands. Find what
implements it and name the files you settled on and how you found them — this is the one
case where *you* choose what gets verified, and the reader needs to see that choice. If
nothing plausibly implements the subject, say so and stop.

**Nothing** — `~/.claude/scripts/resolve-scope.sh resolve` with no `--scope`: uncommitted
work, else the branch against its merge base, else `git show HEAD`, per
`~/.claude/reference/scope-resolution.md`. Where nothing resolves it says so and stops;
ask for `--base` rather than guessing.

The resolver exits **2** when the scope is prose — the subject case. Any other non-zero
exit is a real failure: report it and stop.

## 2. Classify each claim, not the change

A single PR usually makes several claims of different kinds, and "verified" means
something different for each:

| The claim | Verified means | Read |
| --------- | -------------- | ---- |
| a bug is fixed | the symptom is present on the base and absent on HEAD | `references/differential.md` |
| a capability is new | it works, its unhappy paths fail correctly, and what worked before still works | `references/expectations.md` |
| nothing observable changed (refactor) | the same input produces byte-identical output on both trees | `references/differential.md` |
| it is faster | a threshold stated in advance, met across distributions rather than a single pair of numbers | `references/differential.md` |
| the prose is accurate | each procedure it gives runs, and each behaviour it describes happens; claims settled by reading are review's | `references/expectations.md` |
| a subject behaves (no diff) | the contract holds, including its refusals | `references/expectations.md` |
| it is safe to deploy | it survives the window where two versions run at once, and it can be rolled back | `references/compatibility.md` |
| the branch is complete | a clean tree of HEAD installs, builds, boots and passes without your uncommitted state | `references/environments.md` |

What the live system is doing — real traffic on the changed path, what is already
failing — this skill cannot see; `/wtf-prod-impact` reads that and runs nothing.

## 3. Write the expectations down, then show them

Each expectation pairs an input or action with an observable someone who never read the
code could check: a status code, a response field, an exit code, a log line, a row that
is or is not there afterwards. "The cache is no longer invalidated twice" is not an
observable. "The second request returns 200 with the updated title instead of the stale
one" is.

**Start from the review, if there was one.** A finding like "this could 500 on a null
slug" is already an input with a predicted observable. A finding that turns out wrong is
as useful to the author as one that turns out real.

Three kinds, and the second is where the bugs are:

- **positive** — the thing works. The part that was always going to pass.
- **negative** — the inputs that must be *refused*, and refused correctly: the 400 that
  should not be a 500, the auth check that must not fail open. Name *how* it fails, and
  how you will produce the condition. `references/expectations.md` has the catalogue and
  the mechanisms.
- **regression** — what worked before still works.

Also, from `references/expectations.md`:

- **What a run leaves behind is an observable.** Snapshot the state, run, diff it — a
  handler that validates after it writes returns the same 400 either way.
- **One row per independently checkable clause.** Rows that share a command are not
  one row.
- **Where the change states an invariant, the row is the property**, over generated input.
- **One claim per meaningful area of the change**, each at the cheapest tier that can see
  it (§5).

**Check what CI already runs before designing probes** — a probe duplicating a job that
fires on every push has spent minutes to reproduce a green check.
`references/environments.md` has the discovery commands.

### The adversary pass

Where the change has a surface that takes outside input — an API, a CLI, a form, a
parser, a queue consumer, an auth boundary — dispatch `wtf-verify-adversary` with the
Agent tool and give it **only** the diff you resolved, or for a path or subject the files
or area. None of your reasoning and none of your expectations: its value is in the inputs
the author did not imagine. Skip it for a refactor or a prose change.

Merge its cases into your list, dropping any whose expected refusal you cannot state
concretely. A case it marks **unreachable** is not probed and cannot fail the run; list it
under **Left out** with that reason.

### Show the list, then run

The list goes to the user once, as one block, and it is the last thing on the page before
anything runs:

```
**Verifying.** `feat/archived-posts` vs merge-base `2427dfb` — 4 files
**Runs.** Tier 2 — the service from a throwaway worktree of HEAD, compose `db` and `redis`; nothing reaches the network. ~5 min.
**Asks first.** Booting `db` and `redis` is tier 2 spend — say go.
**Left out.** The admin override path — needs a second seeded role.

| # | Kind | Input | Expect | Control | Predict |
| - | ---- | ----- | ------ | ------- | ------- |
| 1 | positive   | `posts(includeArchived: true)` as an editor | 200; archived id 7 present | flag off → id 7 absent | ✅ |
| 2 | negative   | same query, anonymous caller | 401 `UNAUTHENTICATED`; no `data.posts` | valid session → 200 | ✅ |
| 3 | negative   | `includeArchived: "banana"` | 400 naming the field — not 500, not a silent false | valid value → 200 | ❌ coerces to false |
| 4 | regression | `posts` with no new argument | archived rows absent, as before | baseline `2427dfb` | ✅ |
```

One line per cell; the mechanism and full observable go in `plan.md`. **Control** is §4's
discriminating partner and **Runs** is §5's tier and isolation, both settled before the
block prints.

**Asks first** is the one place the run stops for the user, and it names every pause or
says `Nothing`. Three things always go there:

- tier 2 or 3 spend — booting services, driving a browser
- a file broken in place in their checkout (§4)
- a ref someone else authored, such as a fork's PR or a contributor's branch. Name whose
  code it is: bootstrapping runs that tree's install scripts with this machine's `.env`
  symlinked in.

Where it names one, the block ends waiting on it. Otherwise run without waiting — the
user can still interrupt with the case you missed.

**Predict** is written before execution, because afterwards everything looks like
confirmation: a 500 reads as "rejected", an empty array as "filtered correctly". It says
whether you expect the row met and, for a ❌, why.

**Rows keep their number to the end of the run** — progress lines, result table and
promotion list all say `#3`. A progress line names the rows (`probing: #1 #3 #4 done,
running #2 #5 #6`) and goes out when that changes, not on every wake.

## 4. Give every behavioural probe a discriminating partner

A probe you have only ever seen green is not evidence. The exception is a row whose claim
is only that a command runs — "compiles with warnings as errors" — which is settled by a
fresh capture meeting its declared result; `0 tests` is still not a pass when tests were
asked for. For everything else, pick one, cheapest first:

1. **The negative case is its own control.** If the invalid input produces the same
   result as the valid one, the code is not reading it.
2. **Break it deliberately.** Flip the flag off, drop the header, invert the condition,
   and confirm the probe goes red for the reason you expect. Break inside a worktree you
   can throw away. If it has to be in the user's checkout, it goes under **Asks first**,
   and no git command undoes it: `git checkout` or `git apply -R` would also discard their
   own edits to the file. Copy the file to `"$OUT/pre-break"` and set a `trap ... EXIT` to
   copy it back *before* editing, so an interrupted run still restores it. Then `cmp` the
   restored file against the copy, and say that it matched.
3. **Run it against the base.** The full differential, and the only form that shows *the
   change* caused the difference — `references/differential.md`.

A bugfix claim needs (3). A new capability usually takes (1) plus (2), because a base that
lacks the surface tells you nothing about whether HEAD gets it right. A prose claim is a
procedure or a behaviour and takes the partner its kind would. When the probe only runs a
test this change adds, (1) checks the test rather than the code: use (2) or (3).

`references/differential.md` has the probe rules. Above all, the probe must run on both
sides: a baseline failing with `Cannot find module` reads as the bug reproducing. Run it
twice on each side you draw a conclusion from, because non-determinism is
indistinguishable from a fix.

## 5. Pick the cheapest tier, and the isolation to match

Escalate only when the tier below is blind to the symptom; each step up adds ways for the
environment, rather than the code, to decide the answer.

| Tier | Environment | Cost | Use when the claim lives in |
| ---- | ----------- | ---- | --------------------------- |
| 0 | a test in one package or module | seconds | logic, contracts, transforms, parsers |
| 1 | headless script, CLI, or a real build/render | ~1 min | pipelines, cross-package wiring, generated output |
| 2 | the service booted, driven over its real interface | minutes | API shape, persistence, auth, background jobs |
| 3 | full stack and a browser | many minutes | UI, hydration, anything visual |

`--tier N` pins the tier; `--base <ref>` names the comparison point for a differential.

| You need | Use | Because |
| -------- | --- | ------- |
| to run two versions of the tree | a second worktree — `~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh` | a fresh worktree has none of the gitignored state that makes your checkout unreproducible |
| a probe that writes state | the project's own compose file, test database or fixtures | shared mutable state between runs measures your data, not your code |
| only HEAD, at tier 0–1 | your checkout | nothing to isolate from |

**Never point a probe at a shared environment** — not staging, not production, not a
colleague's instance. Before any probe that can send something outward (email, SMS,
webhook, payment, a third-party write), confirm you are in the project's sandbox or mock
mode: an accidental send has no cleanup at all. Do not build a container the project does
not already have; use its compose file or devcontainer, or drop a tier.
`references/environments.md` has the detail per tier, language and isolation mechanism.

## 6. Run

**Turn the instrumentation up before the first run** — strict unhandled rejections,
warnings as errors, backtraces, statement logging — identically on both sides of a
differential. **Run under the project's coverage tool in the same pass**, so §9's
**Covered** line measures the bytes the report quotes. A timing probe is the exception to
both: it runs alone, quiet and uninstrumented. `references/environments.md` has the flags
and the coverage invocation per ecosystem.

Capture raw stdout, stderr and the exit code to files, verbatim, then **read the whole
capture, not the line you predicted** — the unhandled rejection under a passing
assertion, the error from a background job no caller saw, the query log that grew with
the fixture. `references/evidence.md` has the layout and what to read for.

Serialize anything that binds a port or touches a database, and give each run a fresh
entity or a reset between.

## 7. Adjudicate

| Verdict | When |
| ------- | ---- |
| **Verified** | every expectation met — each behavioural row with a discriminating partner that behaved as predicted, each command row on a fresh capture |
| **Verified with gaps** | what you ran passed, but part of the change or subject went unexercised — name the part |
| **Not verified** | the probe could not discriminate: green on the base too, red for environmental reasons, or non-deterministic. Say which of the three, and what would tell them apart |
| **Falsified** | an expectation failed for a real reason. A defect, found before the merge |

### Check each green before reporting it

The probe's author is the reader least able to see that it would have passed anyway. So
before a behavioural row goes green, try to make it meaningless. It is demoted to **Not
verified**, with the reason carried in its row, if any of these holds:

- **the probe never reached the change** — wrong endpoint or argument, a cached response, a
  stale build. Something in the capture must be producible only by the changed path
- **the assertion is satisfied by anything** — a grep that also matches the error body, a
  `200` any route returns, an empty array from a query that returned nothing, `0 tests` with
  exit 0
- **the discriminator does not discriminate** — the negative case fails for an unrelated
  reason, the break was made in a file the run never loads, the baseline failed to build
- **it was green on the base too**, or, for a property, the generator never reached the
  changed branch
- **the expectation was read off the implementation** — the tautology trap in
  `references/expectations.md`
- **the environment answered** — a row an earlier probe left, a warmed cache, a value from
  `.env`

Where you cannot tell, demote: an undefendable green is what this check exists to catch.
A row that came out red for its predicted reason has already discriminated, and a command
row (§4) has no discriminator to check.

`Not verified` is neither a pass nor a defect. Never loosen a probe until it finally goes
red — `references/differential.md` has why that is the worst outcome this method can
produce.

**Falsified is a success for this process.** Report the failure with its reproduction
first, then offer to fix it and re-verify — ask; do not assume. After a fix, the terminal
report keeps the original failure and the PR section describes the code as it now stands;
`references/evidence.md` has both forms.

## 8. Is it guarded, and what is worth keeping

Do this before the report, because it builds a worktree pair the report's **Residue** line
must account for.

Break the line the change turns on in a worktree of HEAD and run the project's **own**
test command, unscoped. Green means nothing in the suite guards the change — the report's
**Guarded** line, and the strongest argument for promoting a probe. Where the change has
no executable line to break — prose, a config the run never loads — say so and skip it.
`references/promotion.md` has the mechanics, then tear the pair down with
`baseline-worktree.sh remove`.

Then triage: promote a probe when it is deterministic, cheap, fits the project's existing
harness and would catch this regression again; leave it throwaway otherwise. Offering to
promote everything is how a suite gets slow, flaky and ignored.
`references/promotion.md` has the table.

## 9. Report

`references/evidence.md` has the terminal and PR forms. In order: the verdict in one line;
**To fix** — every ❌ row, ordered by what happens if it ships, or `Nothing`; **Keep as
tests** — §8's triage, every row accounted for; the plan's table again with results; then
**Covered**, **Guarded**, **CI** and **Residue**; and one closing question.

**Every trailing line is observed or marked as an assumption.** No probe produces them,
so each needs its own command: **Covered** from §6's coverage run over the changed lines,
**Guarded** from §8, **CI** from reading `.github/workflows`, and **Residue** from
`git status`, `docker ps` and `baseline-worktree.sh path` *after* teardown. Where you
could not check, write *assumed, not checked*.

Write the PR verification section to the scratch directory, tell the user the path, and
offer to post it. Ask before posting: it is public. It goes in the PR body between the
`verify:start` / `verify:end` markers and touches **nothing else on the PR** — the
ownership rule in `~/.claude/reference/github-publishing.md`, with the channel mechanics
in `evidence.md`. Post the whole section, not the verdict line: what a reviewer can use is
the discriminators and what went uncovered.

If you fixed code at §7 and the user declines to post, a section already on the PR is now
stale. Read the body for `<!-- verify:start -->` and say so, or say the read failed.

## 10. Promote on a yes

Write nothing until the user agrees. Then rewrite each probe in the project's own idiom,
matching a neighbouring test. Prove it fails against the code that lacked the fix — in a
rebuilt pair, removed again afterwards so the report's **Residue** line stays true — and
run the *unscoped* test command once to confirm the default selector collects it.
`references/promotion.md` has where tests live per ecosystem, and the substitute check for
a new capability the base cannot exercise.
