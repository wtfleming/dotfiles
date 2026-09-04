Claude Code config
==================

Everything under here is deployed to `~/.claude/` by `sync-dotfiles.sh`, so it
applies in every repo, not just this one. This README is documentation for the
repo and is deliberately not synced.

## Code review

`/wtf-code-review` reviews recent changes in a **fresh context** — a subagent
that never sees the conversation which wrote the code, so it cannot inherit the
author's assumptions about it. Given a subject instead of a revision it reviews
that code as it stands, picks the files itself, and says which ones it picked.

```
/wtf-code-review                     # uncommitted, else the branch, else HEAD
/wtf-code-review HEAD~3              # any ref, branch or path
/wtf-code-review main --deep         # add a verified parallel pass per dimension
/wtf-code-review "db connection"     # a subject, reviewed as it stands
```

Without `--deep` it settles the scope, runs the project's test suite and linter,
reviews the diff against the checklist, and prints findings as Critical, Warning
or Suggestion — then stops. Any Critical is checked by a `wtf-refuter` before the
report prints, since a false Critical stops work that should not stop; most runs
have none and so spend nothing. Warnings arrive marked `(unverified)`. The reviewer has no `Edit` or `Write`, so a review
cannot change anything.

`--deep` adds up to eight `wtf-lens` agents in parallel, one per lens: correctness,
security, tests, maintainability, performance, dependencies, reuse, resilience. A
change that touches only prose skips the four lenses with no surface there
(tests, resilience, performance, dependencies), decided from the file list alone
and disclosed in the report. That list comes from the scope artifact rather than
from a second set of git commands, so it cannot disagree with the diff the lenses
read. Their reports are merged and deduplicated with the reviewer's, then verified
before printing: one `wtf-refuter` per Critical and Warning finding, each told to
argue the finding is *wrong* and to answer refuted when unsure.

The merge is where two lenses finding one defect becomes one line, and it used to
be the single place in the command where a finding could vanish without the
reader being told — everywhere else, "a dropped finding is reported, not hidden".
So every candidate now gets a disposition, `kept`, `merged` or `dropped`, and the
report prints the ones that are not in it. Each finding also carries the lens that
raised it, which is what a disposition refers to and what shows which lens earned
its dispatch. Collisions run down an ordered ladder — Pre-existing, then tier,
then the statement naming a concrete failure, then the reviewer over a lens, then
the longer evidence — because "keep the more specific statement" leaves two
equally-tiered findings with nothing to separate them, and the model then picks by
feel between reports its own agents wrote. Suggestions arrive marked
`(unverified)` rather than spending an agent apiece on nits. There is
deliberately no linter lens — the reviewer already runs the real one.

`reuse` and `resilience` are the two lenses with no counterpart in the checklist.

`reuse` is also the only one whose target sits outside the diff — both the
duplicate it hunts for and the code the change orphaned live in files the change
did not touch — which is why it owns both halves rather than splitting the orphan
case into `maintainability`. One lens, one evidence bar: search before asserting an
absence, cite what you found by `file:line`, and judge duplication by whether the
two copies have to change together rather than by how alike they look.

`resilience` asks what happens when something the code *calls* fails, hangs or
half-succeeds: missing timeouts, retries without backoff, failures swallowed into
a default that reads as success, half-completed work that leaves inconsistent
state.

Both of its neighbours get an explicit boundary in the command, since a lens that
bleeds into the one beside it produces the same finding twice in different words.
`correctness` keeps whether the code computes the right answer from the inputs it
was given. `performance` and `resilience` divide by path — the cost of the happy
path against the behaviour of the failure path.

There is no fix flag; the review itself never edits. The report lands in the
conversation, so to act on it, say which findings — "fix the first two" — and
the fixes happen in the main session, which knows what you were trying to do.
Each fixed Critical or Warning is then checked by one fresh `wtf-refuter`
arguing against the fixed tree, so the fixes get the same independent
verification the findings did. Committing stays yours.

### The single-agent variant

`/wtf-code-review-no-lenses` is the same command with the lenses folded back
into the reviewer. Under `--deep` it dispatches no `wtf-lens` at all: the eight
rubrics ride along in the reviewer's own prompt, and the one agent that already
diffed the change and read its files works through them in one pass. It exists
because most of a `--deep` run's tokens go on eight agents each re-reading the
same scope before any of them writes a line.

```
/wtf-code-review-no-lenses main --deep
```

Everything downstream is unchanged — same report, same promotion rule, same
`wtf-refuter` per Critical and Warning. What it gives up is the isolation: eight
agents each with one rubric and a full context budget go deeper than one agent
holding eight, and a lens that finds nothing is a fact about coverage that a
single reviewer has to be asked for. So it asks: the reviewer closes with a
**Dimensions** section accounting for each rubric as findings, `no findings` or
`not applicable`, and the command relays it rather than filling in the gaps.

The two are meant to be run against the same PR and compared, so keep them in
step. What differs is the dispatch machinery and nothing else: spawning the
lenses, `Pick the lenses`, and the two-round launch on a subject scope. The
rubric table and everything downstream of it — the promotion rule, the verify
pass, the triage, the fix and GitHub sections — belong in both, and a change to
one of those is a change to both. Retuning a rubric row is the case to watch:
it reads like a change about the lenses, and it is not.

### Design review, earlier in the cycle

`/wtf-design-review` asks a different question at a different time: *is this
change the right shape?* Run it mid-work, while changing course is still cheap —
`/wtf-code-review` is the pre-PR gate, and design feedback that arrives at
the gate arrives after the sunk cost.

```
/wtf-design-review                   # uncommitted, else the branch, else HEAD
/wtf-design-review src/sync         # any ref, branch or path
```

It dispatches a single `wtf-design-reviewer` agent, cold, with the scope and
nothing else — no summary of intent, no rejected alternatives — so it reviews
the shape rather than the rationale. Output is **Suggestion-only** (design
advice never blocks), and every finding must name a concrete, smaller
alternative and what it buys; "consider making this more modular" is banned by
the agent's own rules. It does not run tests or the linter — mid-work they are
allowed to be red — and it does not hunt bugs.

### The agents

| Agent | Role |
|---|---|
| `wtf-change-reviewer` | scope, tests, lint, the full review |
| `wtf-lens` | one dimension only; dispatched up to eight times by `--deep` |
| `wtf-refuter` | tries to kill a single finding |
| `wtf-design-reviewer` | shape of the change, Suggestion-only; dispatched by `/wtf-design-review` |

All four are read-only — no `Edit`, no `Write`, and no ability to spawn an agent
that has them. Edits only ever happen in the main session, one approval at a time.

### Tuning it

- `reference/code-review-checklist.md` sets the priority order. The reviewer
  reads the deployed copy at review time, so editing it changes behaviour
  without touching an agent definition.
- A project's own `REVIEW.md`, `AGENTS.md` or `CLAUDE.md` wins where it conflicts.
  `REVIEW.md` is the name Anthropic's own code review reads.
- The eight `--deep` rubrics live in the commands, not in `wtf-lens`, so they can
  be retuned without editing an agent — but there are two copies of the table
  now, one per command, and a retune means editing both.

### Cost

`--deep` spawns one reviewer, up to eight lenses, and one refuter per verified finding —
tens of agents on a real branch — and asking for fixes afterwards adds one more
refuter per fixed Critical or Warning. It announces each fan-out before spawning
it, so the spend can be refused. For very large diffs, the built-in `/code-review ultra`
is the maintained alternative.

## Resolving the scope

`scripts/resolve-scope.sh` answers, once and in code, the two questions every review tool
here has to settle before it reads a line: **what code is under review**, and **does the
working tree actually hold it**.

The first matters because `--deep` dispatches eight lenses plus one refuter per finding,
each of which used to run its own git commands. "The same scope" then held only for as
long as every agent derived it identically — an instruction, not a fact. Now the diff is
produced once, written to `scope.diff`, and handed over by path, with `manifest.json`
carrying the file list projected from that same diff.

The second is the sharper one, because it fails in one direction only. Reviewing `HEAD~3`,
or a fetched PR, is ordinary, and in both the files on disk are not the files under
review. `wtf-refuter` reads the working tree by default and answers `refuted` when it
cannot decide — so a refuter pointed at the wrong tree does not find the line a finding
names, cannot decide, and kills it. The mismatch does not add noise; it silently deletes
true findings. The manifest reports one of six correspondence states, distinguishing the
two ancestor directions and keeping "not present locally" separate from "divergent", and
agents read the scope's blobs rather than the working file whenever the tree is not the
scope. A line that is not in the working tree is not a refutation.

It also folds untracked files into the same diff, fetches the base before computing a
merge base, resolves the default branch rather than assuming `main`, and treats
`gh pr diff` as the sole authority on a PR — a failure there is a stop, never a fallback
to a locally computed range, which would review a near-miss of the PR while claiming to
review the PR.

One invariant the consumers lean on: **if `manifest.json` exists, the scope is non-empty.**
An empty diff on disk is indistinguishable from a collapsed range, so every path that
would describe an empty scope either falls through to the next step or exits without
writing anything, recording in `fell_through` what each step found. A scope that is not a
PR, a range, a ref or a path exits 2, which is how a caller tells a subject from a failure
without parsing an error string.

Resolving before dispatch also removed a round: naming no scope at all used to wait for
the reviewer to settle it. A prose subject is now the only shape that still runs in two.

## Interactive explainers

`wtf-explainer` builds interactive, self-narrating explainers of complex
technical systems — a real simulation underneath, station-by-station narration
on top, in one dependency-free static page. Two forms: an isometric town a
vehicle drives through (the RollerCoaster-Tycoon-style tour), and a flat 2D
schematic with a focus ring for architectures where many things move at once.

It triggers on requests like "build an interactive explainer for how X works"
or "visualise this pipeline"; a single chart is `dataviz`'s job and a static
diagram is `artifact-diagramming`'s.

Adapted from [learnscape](https://github.com/LaurentiuGabriel/learnscape)
(MIT). The PacketPost template under `assets/template/` is vendored from
upstream **with local fixes** where review found defects — `main.js`
(browser-shortcut and pinch guards, fly-to that arrives), `sim.js` (tour
completion aware of the cache-hit branch), `ui.js` (pinned card and live
plan), `index.html` (ledger wording) — so refreshing those files from
upstream silently reverts the fixes; the skill directory's git history is
the divergence record. The skill's build order still has projects copy the
engine files rather than rewrite them. Local additions: a generalized
SKILL.md, a knowledge-base phase before any code (`NOTES.md`, reviewed for
accuracy, feeding the fidelity ledger), the flat-schematic form
(`references/flat-format.md`), and a verification fallback via the
chrome-devtools MCP for machines without Playwright.

## Verifying that it works

`wtf-code-verify` proves that code does what it is supposed to do, by running it. The
line against its neighbour is deliberate: **`wtf-code-review` reads the code, and
`wtf-code-verify` executes it.** Given a scope with nothing runnable and no claim
checkable against a running system, it says so and stops rather than sliding into a
review.

It assumes `/wtf-code-review` has already run over the same scope, which is how it is
normally used, so it does not re-report what a reader could have found. It starts instead
from the review's *unverified* findings — the best expectations available, since someone
already thought each one was suspicious and nobody settled it either way. Review produces
hypotheses; this closes them, and a finding that turns out to be wrong is as useful to the
author as one that turns out to be real.

The idea it turns on is that a green check proves nothing on its own — it may have been
green before the change, or green against code that does not work. So every probe needs
a **discriminating partner**: the negative case that must come out different, a
deliberate break that must go red, or the full differential against the merge base. The
skill picks the cheapest one that carries the claim.

Expectations are written down and shown to you *before* anything runs, in three kinds —
positive, negative, regression — because a probe built on the wrong idea of correct runs
cleanly, passes, and tells you nothing. The negative cases are dispatched to the
`wtf-verify-adversary` agent, which sees only the diff: whoever wrote the code has
already imagined the inputs it handles, and the bugs are in the ones they did not.

Beyond behaviour it covers the things only execution reveals. `references/compatibility.md`
handles the window where two versions coexist — new code against the old schema and old
code against the new one, old clients against the new server, jobs enqueued by the
previous version, and whether the down migration has ever actually been run. It asks
whether a clean tree of the branch still builds and boots, which catches the dependency
in the lockfile but not the manifest and the env var documented nowhere. It measures
scale instead of eyeballing it, since an N+1 shows up as a query count of 3 + N where
wall-clock is noise. And on a PR it verifies the title and description in both
directions: every claim in the body true, *and* every meaningful change accounted for —
because what review produces is usually an omission nobody went back to write up, and a
squash merge makes that title the permanent commit subject on `main`.

It reports one of four verdicts. `Not verified` is neither a pass nor a defect and says
which of the three inconclusive shapes it was; `Falsified` — a real defect, found before
the merge — is a success for the process, and it offers to fix and re-verify rather than
quietly rewriting history. Every report carries what went uncovered, what CI already
runs, and what state was left behind — and each of those lines is something checked
rather than recalled. A verification section is read as the output of a process that
checked things, so a sentence in it that was reasoned rather than run borrows that
authority without earning it. Figures fall under the same rule: a count taken from raw
text rather than a parse reads exactly like a measured value and is not one. It ends by triaging the probes and offering to
promote the ones worth keeping into permanent tests, in the project's own idiom, proven
to fail without the fix.

Scope can be a ref, a branch, a PR, a path, or plain prose — `/wtf-code-verify login and
authentication` verifies an area of a running system with no diff at all, and a docs
change is verified by treating the code as the source of truth and each written claim as
the subject under test.

It absorbed the earlier `wtf-verify-fix`, whose merge-base differential is now one
technique among several in `references/differential.md` — the two would otherwise have
competed for the same trigger. `scripts/baseline-worktree.sh` and
`references/environments.md` came across with it, and the script gained `--base-exact`:
the merge base is what isolates *your* change, but the deploy window asks what is running
beside you, which is a release tag and usually not an ancestor. Without it that probe had
to hand-roll a worktree and skip the bootstrap the script exists to do.

How to resolve a scope lives in `~/.claude/reference/scope-resolution.md`, shared with the
review tooling rather than restated here, and is implemented by
`scripts/resolve-scope.sh`. Hardcoding `main` fails silently on a `master` or `trunk` repo
— the merge base collapses, the diff comes back empty with exit 0, and an empty diff read
as "no changes" is a confident report about nothing. `baseline-worktree.sh` was the last
place that still hardcoded it, and now takes its default from the same resolver.

Deliberately expensive and deliberately rare: at the upper tiers it boots services and
bootstraps a second worktree, so it announces the tiers it expects to need and asks
before spending tier 2 or 3. Meant to run once on a finished branch just before the PR
opens — not per commit, and not as a substitute for running the tests.

## Opening the PR

`/wtf-create-pr` composes a pull request and opens it. The line it holds is the same one
the two tools above hold against each other: it does not review the code, does not prove
it runs, and does not merge. A PR-opening command is tempting to load up with all three,
and a version that hunts for defects gets invoked at the wrong point in the cycle — early,
while the branch is still moving — while a version that gates on its own findings turns
"open a PR" into an argument.

The mechanical pre-flight is the cheap part: HEAD is a branch and not the default one
(resolved, not assumed), the remote-tracking ref is refreshed before the branch is judged
pushed and current, no PR already exists for it, uncommitted and untracked work is named
rather than silently left out, and a `pull_request_template.md` is filled in rather than
replaced. Those usually pass. Two of them are there because the failure is silent rather
than loud: without the fetch, a branch pushed from another machine reads as in-sync while
the remote head carries commits the body never describes; and on a detached HEAD the branch
name is empty, which `gh pr list --head ""` treats as *no filter* rather than *no match* —
it returns every PR in the repo, and the existing-PR check then reports a stranger's PR as
this branch's. The push itself is named as the irreversible step, since it happens in
pre-flight and the confirmation gate later covers only the title and body.

The part that actually finds things is drift — prose that was true when it was written and
stopped being true while the branch moved. Three axes: documentation describing the changed
area, a review or verification that ran at a commit the branch has since left behind, and
a `VERIFICATION.md` whose recorded SHA no longer matches HEAD. Each is judgement rather
than a checklist, and each takes the same two-direction test the body gets — every claim
true, *and* every meaningful change accounted for. The second direction is the one that
catches things, because nothing in stale prose is false; it is wrong by omission, and an
omission does not announce itself. On a short branch written in an afternoon nothing has
had time to go stale, and the honest output is one line saying so: a check that
manufactures findings to look useful spends the next real finding's credibility.

None of it is a gate. "The bot has not reviewed the tip" is information for the author, not
grounds to refuse — it reports, then opens.

The title gets judged separately from its conventional-commit prefix, because on a squash
merge it becomes the permanent commit subject on the default branch and a `fix:` that grew
into a `feat:` during the branch is a wrong version bump rather than a wording nit. Visual
evidence passes two gates before it is attached: is there a *before* to compare against,
and is the repo private — GitHub serves public-repo attachments from a host readable by
anyone with the link, and a screenshot carries names, avatars, internal hostnames and a
token in the URL bar, none of which can be scrubbed by rule the way text can. Text stays
text; a screenshot of a JSON response is unsearchable, uncopyable and undiffable.

How much of that to do is decided by the base, which is the one thing that separates a PR
in the middle of a stack from the PR that lands the work. An intermediate PR is reviewed as
a slice, absorbed into its parent and superseded by the one that finally targets the default
branch, so effort spent making it a finished public artifact is thrown away when the stack
lands — visual evidence, the prose-drift pass and an embedded verification section all wait
for the final PR. Two things stop being true mid-stack rather than merely costing more: the
title is absorbed instead of becoming a permanent commit subject, so judging it as one is
cargo cult; and a closing keyword is inert, because GitHub closes a linked issue only when
the PR merges into the default branch. Inert in the way that reads as done is how a ticket
ends up neither closed nor tracked, so the id goes on the intermediate PR and the keyword on
the one that reaches `main`. Stack position is detected — an open PR whose head is an
ancestor of HEAD — rather than guessed, because basing a stacked PR on the default branch
makes it claim its parent's commits and renders the diff unreviewable.

A ticket reference is found rather than invented: from the argument, the branch name, the
commit trailers, or a template's issue line, and never inferred from the subject matter,
since a mistyped id with a keyword in front of it closes someone else's ticket. It goes in
both places for different reasons — the title, because that is what survives in `git log`,
and the body, because that is the only place a closing keyword fires at all. A partial fix
references without closing.

It shows the title and body before anything is public, and stops after reporting the URL.

`~/.claude/reference/github-publishing.md` holds the guards that apply to all three tools
that write outward — scrubbing text, looking at every image, delimiting a generated section
so a rerun replaces it instead of stacking a second verdict below the first, and keeping
what cannot be regenerated in a comment rather than in a body that will be. Extracted
rather than written a third time, so a fix lands once.
