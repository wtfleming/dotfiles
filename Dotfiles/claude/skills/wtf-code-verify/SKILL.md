---
name: wtf-code-verify
description: 'Prove that code does what it is supposed to do before it merges, by executing it — derive the falsifiable expectations a change or a subject implies, probe each one including the inputs that must be refused, and report a verdict backed by raw output. Use this whenever a PR is about to be opened or changes are about to merge, and whenever the user asks to verify, prove, confirm or sanity-check that something actually works: "does this actually work", "verify the fix", "e2e test this change", "spin up the server and check it", "make sure auth still works before I merge". Takes a ref, branch, PR or a prose subject — "login and authentication" — so it also covers an area of a running system with no diff, and whether docs, comments or a PR''s own title and description still match the code. Expensive by design — it boots services and builds a second worktree. NOT for reading code without running it (that is wtf-code-review), routine test runs, or checking unfinished work mid-development.'
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
Do not slide into reading the code and presenting that as verification — a reader cannot
tell the two reports apart once they are written.

Assume review has already run over this scope, because it usually has. So **do not
re-report what a reader could have found**: the missing guard, the awkward name, the
suspicious-looking query. Everything here has to be something only execution could tell
you. Confirming a defect review already suspected is worth doing — that is turning a
suspicion into a fact — but hunting for read-findable defects is work someone else
already did, in a report the user is holding.

Read each reference only when a claim calls for it:

| Reference | Read it when |
| --------- | ------------ |
| `references/expectations.md` | working out what "correct" means — per claim type, plus the catalogue of cases that should fail |
| `references/differential.md` | the claim *is* a difference: a bugfix, a refactor's equivalence, a perf threshold |
| `references/compatibility.md` | the change touches persisted state, a deploy boundary, or anything another system consumes |
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

And before bootstrapping or booting a ref you did not author — a fork's PR, a
contributor's branch — say whose code is about to run and get explicit confirmation.
Bootstrapping executes that tree's own install scripts in a worktree with this machine's
`.env` symlinked in, so it is arbitrary code with live credentials to hand.

## 1. Establish the scope

A named scope comes in three shapes.

**A revision** — a ref, a branch, a path, a PR number. Resolve it with
`~/.claude/scripts/resolve-scope.sh resolve --scope <it>`, the same way an omitted scope is
resolved below, so a named revision gets the artifact and the `correspondence` too rather
than a diff derived a second way. `gh pr view <n>` still gives you the body, and the body
is where the author already wrote down what they think they did. On a PR that body is both context
and a claim in its own right, verified alongside the code — it was written before review
and is rarely updated after.

**A subject** — prose naming an area of behaviour, such as `login and authentication`.
There is no diff and no author here: you are verifying a contract as it stands. Find
what implements the subject, name the files you settled on and say how you found them.
A subject scope is the one case where *you* choose what gets verified, and a reader who
cannot see that choice has no way to tell whether the run covered the thing they meant.
If nothing in the repo plausibly implements the subject, say so and stop rather than
verifying the nearest thing you found.

**Nothing** — resolve it with `~/.claude/scripts/resolve-scope.sh resolve` and no
`--scope`: uncommitted work, else the branch against its merge base, else `git show HEAD`. It implements
`~/.claude/reference/scope-resolution.md`, including the two things that decide whether
this step is silently wrong — the default branch resolved rather than assumed to be
`main`, and an empty diff treated as *fall through* rather than *no changes*. Where
nothing resolves it says so and stops; ask for `--base` rather than guessing.

The resolver exits **2** when the scope is prose — that is the subject case above, and
its procedure applies instead. Any other non-zero exit is a real failure: report it and
stop rather than verifying something else.

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
| the PR describes the change | every claim in the title and body holds, *and* every meaningful change is accounted for | `references/expectations.md` |
| it is safe to deploy | it survives the window where two versions run at once, and it can be rolled back | `references/compatibility.md` |
| the branch is complete | a clean tree of HEAD installs, builds, boots and passes without your uncommitted state | `references/environments.md` |

## 3. Write the expectations down, then show them

Before running anything, write the list. Each line pairs an input or action with an
observable — something you could show to someone who has never read the code: a status
code, a response field, an exit code, a rendered file, a log line, a pixel, a row that
is or is not there afterwards. "The cache is no longer invalidated twice" is not an
observable. "The second request returns 200 with the updated title instead of the stale
one" is.

**Start from the review, if there was one.** A surviving review finding is the best
expectation available: someone already thought it was suspicious, and at the tiers a
review refutes, a refuter argued against it and lost. A Warning reading "this could 500
on a null slug" is a hypothesis with an input and a predicted observable already
attached — which is exactly the shape of a line below. A finding still marked
**(unverified)** is worth more rather than less: it means nobody settled it either way,
because the finding sits below the tiers a review sends to a refuter at all, because the
refuter cap bound before reaching it, or because the agent it was sent to failed. Review
produces hypotheses; this is the tool that closes them, and a finding that turns out to
be wrong is as useful to the author as one that turns out to be real.

Three kinds, and the second is where the bugs actually are:

- **positive** — the thing works. The part that was always going to pass.
- **negative** — the inputs that must be *refused*, and refused correctly. A 500 where a
  400 belongs, a bad parameter silently defaulting instead of erroring, an auth check
  that fails open, a stack trace in the response body. `references/expectations.md`
  carries the catalogue; the point is that the expectation names *how* it fails, not just that it does.
  It carries how to *produce* the conditions too — an expired credential, a dependency that
  is slow rather than down, twenty callers at once — because a case with no mechanism beside
  it is a case that quietly drops out between being written and being run.
- **regression** — what worked before still works. The change's blast radius.

**The response is not the whole observable.** What a run leaves behind is the other half —
the duplicate row, the send on a path that was supposed to refuse — and none of it reaches
the caller, so no assertion on the response can be falsified by any of it. Snapshot the
state, run, diff it: `references/expectations.md` has the rest of the list, and why the
negative cases are where it pays.

Cover a claim per meaningful area of the change rather than one probe for the whole
thing, and pick the cheapest tier that can see each claim (§5) so breadth stays
affordable. `references/expectations.md` has why, and what to say when the list runs past
what the budget allows.

**Check what CI already runs before designing probes.** The probes worth building are the
ones CI does *not* run; one that duplicates a job firing on every push has spent minutes
to reproduce a green check. `references/environments.md` has the discovery commands and
what to say about the overlap in the report.

```
| # | Kind | Input | Expected observable |
| - | ---- | ----- | ------------------- |
| 1 | positive   | `posts(includeArchived: true)` as an editor | 200; `data.posts` contains archived id 7 |
| 2 | negative   | same query, anonymous caller | 401 `UNAUTHENTICATED`; `data.posts` absent |
| 3 | negative   | `includeArchived: "banana"` | 400 with a field-level type error — not a 500, not a silent default to false |
| 4 | regression | `posts` with no new argument | unchanged: archived rows absent |
```

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

### Show the list, then predict

Merge the adversary's cases first — the point of showing the list is to catch a wrong
idea of correct, and a list still missing its negative cases is the half least likely to
be right.

**Show the list to the user before running anything.** Their knowledge of the domain is
exactly what fills the gap you cannot see — "you forgot that the token can be expired
rather than missing" — and one message is far cheaper than a wasted tier-2 run built on
the wrong idea of correct.

**Predict each result before executing it.** Write the expected string down. Afterwards
everything looks like confirmation: a 500 reads as "rejected", an empty array reads as
"filtered correctly", a timeout reads as "slow but working". A prediction on the page is
the only thing that makes those wrong later.

## 4. Give every probe a discriminating partner

A probe you have only ever seen green is not evidence. Three ways to earn discrimination,
cheapest first — pick one per claim:

1. **The negative case is its own control.** If the invalid input produces the same
   result as the valid one, the code is not reading it. Free: you already wrote both.
2. **Break it deliberately.** Corrupt the input, drop the header, flip the flag off,
   point at an id that does not exist. Confirm the probe goes red, and red for the reason
   you expect. Seconds, no worktree — but it has to come back out, and **every git-shaped
   way of undoing it is wrong on a file the user was already working in.**
   `git checkout -- <path>` discards their edits along with the break; so does
   reverse-applying `git diff -- <path>`, because that diff contains both and `git apply -R`
   removes the whole of it. §0 only requires a commit for claims that use a differential, so
   a dirty file is reachable here.

   Break inside a worktree you can throw away. If it genuinely has to be in place, **name
   the file and get the user's agreement before editing it** — the only edit in this skill
   that is meant to be undone, and an unannounced edit to their checkout is
   indistinguishable from a bug in whatever they were working on. The other two writes
   there, §7's fix and §9's promoted tests, are meant to stay; this one is restored
   byte-for-byte below, so leaving it behind is the failure. **Arrange the restore before
   you make the edit, not after the probe** — a `trap ... EXIT` in the shell that does it,
   or an equivalent that survives the probe failing, a later command exiting and the run
   being interrupted. "Copy it back afterwards" describes the happy path only, and the
   paths that skip it are exactly the ones where a broken file is left in the user's
   checkout and committed with the next change. Then `cmp` the restored file against the
   copy and say so, because a restore that silently did not happen looks identical to one
   that did. Copy the file byte-for-byte before touching it and copy it back —
   `cp <path> "$OUT/pre-break"` … `cp "$OUT/pre-break" <path>` — which restores exactly what
   was there without asking git what it thinks the file should look like. A break left
   behind survives an interrupted run as an uncommitted edit nobody attributes, and the next
   `git commit -am` ships it.

   The same break aimed at the project's **own** test suite rather than at your probe
   answers a different question — whether anything guards the change today — and §9 is
   where that one lands.
3. **Run it against the base.** The full differential, and the only form that shows *the
   change* caused the difference. Costs a worktree and a bootstrap —
   `references/differential.md`.

A bugfix claim needs (3): the claim is itself a difference, so nothing weaker can carry
it. A new capability usually takes (1) plus (2), because on the base the surface does not
exist — "the base rejects an argument it has never heard of" is true and tells you
nothing about whether HEAD does the right thing. Prose claims get neither: what
discriminates there is checking the claim against the implementation rather than against
your memory of it.

The trap that produces the most confident wrong answers is **a probe that cannot run on
both sides** — it fails the baseline with `Cannot find module`, and that reads as the bug
reproducing. `references/differential.md` has it, and the three other probe rules.

**Run the probe twice on each side you draw a conclusion from.** Non-determinism is
indistinguishable from a fix, and the baseline is the side where that costs most and the
side you will be tempted to run once. `references/differential.md` has why, and what to
record instead of a reproduction.

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

Isolation buys attributability and containment, and nothing else — so buy only what the
claim needs; isolation you did not need is time you did not have.
`references/environments.md` has the reasoning and the mechanisms.

| You need | Use | Because |
| -------- | --- | ------- |
| to run two versions of the tree | a second worktree — `~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh` | a fresh worktree has none of the gitignored state that makes your checkout unreproducible |
| a probe that writes state | the project's own compose file, test database or fixtures | shared mutable state between runs measures your data, not your code |
| only HEAD, at tier 0–1 | your checkout | nothing to isolate from |

**Never point a probe at a shared environment** — not staging, not production, not a
colleague's instance. And before any probe that can send something outward (email, SMS,
webhook, push notification, payment, a write to a third-party API) find the project's
sandbox or mock mode and confirm you are in it. An accidental send cannot be undone by
tearing down a worktree. It is not the only thing here that outlives the session — a
migration run against a database somebody cared about is the other, and
`references/compatibility.md` says how to keep that one disposable — but it is the one
with no cleanup at all.

Do not build a container the project does not already have; use its compose file or
devcontainer, and drop a tier if it has neither. `references/environments.md` says why,
and covers the detail per tier, per language and per isolation mechanism.

## 6. Run

**Turn the instrumentation up before the first run, not after a surprise.** The flags
decide whether a failure the code already has appears in the bytes you are about to
capture — strict unhandled rejections, warnings as errors, backtraces on, statement
logging in the database. Set them identically on both sides of a differential, since they
change what the output contains. `references/environments.md` has them per language, and
why they are worth setting before anything looks wrong. One exception: a timing claim is
measured with the loud flags **off**, because their cost scales with the thing being timed.

**Run under the project's coverage tool in the same pass.** §8 reports which changed lines
a probe reached, and taking that from a second run over the same probes pays the expensive
part twice and measures bytes other than the ones the report quotes. The timing exception
covers this too: coverage instruments the process or the node, so it costs time the claim
would be measuring. A timing probe runs on its own, quiet and uninstrumented; the coverage
pass answers what the probes reached, which is a different question from how long they
took.
`references/environments.md` has the invocation per ecosystem.

Capture raw stdout, stderr and the exit code to files, verbatim — write the bytes first
and read them second. `references/evidence.md` has the layout, and why summarizing at
capture time is what launders a compile failure into "the expected failure".

**Then read the whole capture, not the line you predicted.** A probe that returned the
expected 200 also produced a stderr stream, a log and an exit code, and nothing else in
this skill will ever look at them. The unhandled rejection under a passing assertion, the
error-level line from a background job that failed where no caller could see, the
deprecation warning that is a scheduled break, the query log that grew with the fixture:
each is a defect only execution reveals, sitting in a file you have already written.
`references/evidence.md` has what to read for and what to do with what you find.

Serialize anything that binds a port or touches a database, and give each run a fresh
entity or a reset between; `references/environments.md` has the tier-2 detail.

## 7. Adjudicate

| Verdict | When |
| ------- | ---- |
| **Verified** | every expectation met, each with a discriminating partner that behaved as predicted |
| **Verified with gaps** | what you ran passed, but part of the change or subject went unexercised — name the part |
| **Not verified** | the probe could not discriminate: green on the base too, red for environmental reasons, or non-deterministic. Say which of the three, and what would tell them apart |
| **Falsified** | an expectation failed for a real reason. A defect, found before the merge |

### Refute the greens

The table above is applied by the same agent that designed the probes, and a probe's
author is the reader least able to see that it would have passed anyway. So before any
row is reported green, dispatch `wtf-verify-refuter` with the Agent tool — one per ✅
row and **in parallel**, since a shared dispatch lets a strong row carry a weak one and a
serial fan-out spends a round-trip per row at the very end of an already long run. Hold
back any two whose probes bind the same port, container name or database, and run those one
at a time: §6's serialisation rule governs a re-run exactly as it governs the first run, and
a refuter whose re-run dies on a collision cannot settle the row, so it falls to its own
tie-break and answers `refuted` — the green becomes *Not verified* for a reason that is
about your scheduling rather than about the code.

Give it the expectation, the discriminator claimed for it, the probe that was run and the
raw captures from every side the probe ran on — a tier-0 or tier-1 probe may only have
run against HEAD, and the refuter is written for that, reading a baseline capture where
one exists. The probe travels because re-running it is the only way it can
settle non-determinism rather than allege it. **Say whose work the tree is**, which §0 has
already established: the refuter treats silence as untrusted and will not re-run anything
on a tree it cannot place, so an unstated provenance silently removes the one check that
separates non-determinism from an allegation. An ordinary run over the user's own branch
says so plainly; a fetched PR or a contributor's branch is named as such, and their
sanction to execute it is relayed only where they gave it. Not your reasoning about them:
that is the thing under test, and it is the same argument that keeps the adversary blind
at §3.

It answers `stands` or `refuted`, and a refuted green is neither a pass nor a defect —
it is a **Not verified** whose reason you now have in writing, so demote the row and
carry the reason rather than dropping it. Skip the pass for a row that came out red: a
probe that failed for the reason it predicted has already discriminated.

`Not verified` is not a pass and not a defect, and the pull towards recording it as one
or the other is strong. Say which of the three it was, and never loosen a probe until it
finally goes red — `references/differential.md` has the adjudication table and why that
last move is the worst outcome this method can produce.

**Falsified is a success for this process.** Finding it now is the entire point of
running before the PR. Report the failure with its reproduction first, then offer to fix
it and re-verify — ask; do not assume. If you do fix and re-run, the **terminal** report
says the verdict describes post-fix code and quotes the original failure verbatim. A
report to the author showing only the final green run has erased the most valuable thing
the run produced.

The PR section is the other audience and takes the opposite rule: it describes the code
as it now stands and leaves the failure out, because the fix is a commit in the branch
the reviewer is already reading. `references/evidence.md` has both forms.

## 8. Report

Lead with the verdict in one line. Then the evidence, then the lines that are worth more
to a reviewer than another passing assertion — `references/evidence.md` has the terminal
and PR forms, and why each of these earns its place:

- **Coverage** — which parts of the change or subject a probe actually executed, and
  which it did not. Measure it rather than recalling it: read §6's coverage result
  against the changed lines. A changed line
  with zero hits is the most useful thing this run can hand a reviewer, and until it is
  measured this is the one line of the report that is a judgement.
  `references/evidence.md` has what to report, `references/environments.md` the invocation.
- **CI overlap** — what already runs on every push, so this run's contribution is legible.
- **Residue** — rows, files, containers, worktrees, ports left behind, or explicitly
  nothing. A setting you turned up on something you did not start counts: a database's
  statement logging, a service's log level. Tear the worktrees down:
  `~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh remove`.
- **PR description** — whether the title and body still describe the change, or what
  drifted. Only when the scope is a PR. Report the drift; do not fix it.
  `references/expectations.md` has what makes that report actionable for the command
  that does.

**Every line in the report is either something you observed or is marked as inference.**
The verdict has a probe behind it by construction, and the raw capture is on disk — but
those four lines have no probe, and they are where an unbacked claim gets in. Each of them
is a fact about the world with a command that establishes it: read `.github/workflows`
before writing what CI covers, run `git status` and `docker ps` and
`baseline-worktree.sh path` before writing *Residue: none*, re-read the PR body before
saying it still describes the change, and take **Covered** from a coverage run over the
changed lines rather than from the probes you meant to write. Where you could not check,
write what you assumed and say it is an assumption.

That is the same standard the rest of this skill applies to the code under test, turned on
the report about it. A verification section is read as the output of a process that
checked things; a sentence in it that was reasoned rather than run borrows that authority
without earning it, and is indistinguishable from the ones that did.

Write the PR verification section to the scratch directory with the template in
`evidence.md`, tell the user the path, and offer to post it. Ask before posting: it is
public and outward-facing, and a verification section landing on the wrong PR is worse
than none at all.

**It goes in the PR body, between the `verify:start` / `verify:end` markers** — that
section, and **nothing else on the PR**. Not the title, not the description: where either
has drifted, report it and `/wtf-create-pr` fixes it. Why the body rather than a comment,
and what a write there may and may not touch, are the ownership rule in
`~/.claude/reference/github-publishing.md`; `evidence.md` has the channel mechanics, the
check for whose PR it is, and when to fall back to a comment.

That boundary is about outward-facing writes and nothing more. Inside the repo this skill
writes at three steps, each on a yes from the user: §4 breaks a file in place and restores
it, §7 offers a fix to tracked source, and §9 writes promoted tests into the project.

**Which is why declining to post has one more consequence.** Editing code where a published
section may already exist obliges the tool to say that verdict is now stale — the rule in
`~/.claude/reference/github-publishing.md`, which names this skill among the tools it binds.
Posting satisfies it, because the section that goes up replaces the stale one. Fixing at §7
and then *not* posting does not: the code has moved and the live section still reads as
current. So on that path, read the body for a `<!-- verify:start -->` section and say it is
stale, the same as a review command would — and where the read fails rather than coming back
without markers, say that instead.

Post the whole section rather than the verdict line. The reviewer is the audience: they
are about to read this diff, and what they can use is which expectations were probed, the
discriminator that makes each result mean something, and — most of all — what the probes
did **not** cover. A bare "Verified ✅" tells them a run happened and gives them nothing to
review with, while reading as a broader endorsement than the run earned.

## 9. Offer to promote the probes

First find out whether anything guards the change today, because the answer decides how
much the triage is worth. Break the line the change turns on — §4's mechanism, in a
worktree of HEAD so nothing has to be restored — and run the project's **own** test command,
unscoped. Green means nothing in the suite guards the change, which is a finding in its
own right and the strongest argument the triage below can make.
`references/promotion.md` has it, including why that answer and the coverage run disagree
in a useful way.

**Where the change has no executable line to break** — prose, a config the run never
loads — there is nothing to guard and nothing to measure. Say so and go straight to the
triage, the way §8's Covered line takes `N/A — no executable lines changed`.

**Run this check before §8 composes its Residue line, not after.** It builds a worktree
pair, so in section order it creates the residue that the line has already declared
absent — in a section that may by then have been posted to the PR — and leaves the next
`create` refusing without `--force`. Take its answer, report it on its own line, tear the
pair down, and compose Residue over a tree that is actually clean.

A probe worth writing is often worth keeping, but not always — and offering to promote
all of them is how a suite gets slow, flaky and eventually ignored. Triage, then ask.

Promote a probe when it is deterministic, cheap, fits the project's existing harness and
would catch this regression again; leave it throwaway otherwise.
`references/promotion.md` has the triage table and the cases each way.

Present it as a numbered list with a recommendation and a reason per probe, then ask.
Write nothing until they say yes.

```
Probes run:
  1. includeArchived=true returns the archived row     → promote: deterministic, 0.3s, fits tests/api/posts_test
  2. includeArchived="banana" returns a 400            → promote: same file, covers the negative contract
  3. Browser check of the archive toggle               → keep throwaway: 3 min, needs a seeded DB, and 1+2 already pin the contract

Promote 1 and 2?
```

On a yes: rewrite each into the project's own idiom — read a neighbouring test and match
it — then prove the promoted test fails against the code that lacked the fix, and run the
project's *unscoped* test command once to confirm its default selector actually collects
it. A test that passes on both sides asserts nothing; one that never runs on push is
worse, because it reads as coverage forever. `references/promotion.md` has the detail,
including where tests live per ecosystem and the substitute check for a capability the
base cannot exercise at all.
