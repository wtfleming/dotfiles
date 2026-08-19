---
description: Independent code review of recent changes in a fresh context — diff, tests, lint, structured report. Pass --deep to add a verified parallel per-dimension pass.
argument-hint: "[ref, branch or path — defaults to uncommitted, else the branch, else HEAD] [--deep]"
allowed-tools: Agent, Read, Grep, Glob, Bash(git:*)
---

Arguments: $ARGUMENTS

Split those into a scope and the optional flag `--deep`. Everything that is not
the flag is the scope, and the scope may be empty.

This command reviews; it does not fix. If the user wants findings acted on,
they will say which ones in a later message, and that happens in the main
conversation where their intent lives — not here.

## Review

Launch the `wtf-change-reviewer` subagent on the scope. Dispatch it with the
Agent tool, `subagent_type: "wtf-change-reviewer"`, and wait for it
(`run_in_background: false`) — except under `--deep` with a named scope, where
this dispatch is held for the batched launch described below so the lenses do
not serialise behind it.

The whole point is that the reviewer starts cold. So the prompt you send it
contains **only** the scope. Do not include:

- your summary of what the change does or why
- which parts you think are fine, or which you are unsure about
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid. Hand over the scope and nothing else. If the scope is empty, say so and
let the agent work out its own.

Without `--deep`, print the report verbatim when it returns. Do not re-rank the
findings, soften them, or defend the code — you are relaying an independent
review, not negotiating with it.

Under `--deep`, do not print it yet. Its findings are about to enter a verify
pass that may retract some of them, and a finding that is about to be retracted
should not get a first airing here any more than in the merged report below.
Say that the reviewer returned and how many findings it brought, and hold the
rest. The relaying rule still applies to the report you eventually print.

**Without `--deep`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, follow **If the user asks for fixes** below.

## With `--deep`

One reviewer covering six dimensions gives some of them a shallower pass than
the others. This adds a dedicated pass per dimension, over the same diff the
reviewer reads — plus `reuse` and `resilience`, which the reviewer's checklist
does not cover at all.

Say up front how many agents you are about to spawn, so the cost is the user's
to refuse before it is spent rather than after.

Dispatch these eight `wtf-lens` subagents **in parallel**, each with the scope
and its own rubric and nothing else. Unlike the reviewer, a lens cannot derive
its own scope, and eight agents each guessing one is how "the same diff" stops
being true — so where the scope comes from depends on what the user gave:

- **The user named a scope:** every lens gets it verbatim, and nothing a lens
  does depends on the reviewer's output — so launch all eight *alongside* the
  reviewer, in the same batch, rather than after it. The reviewer's test run is
  the long pole of the whole pass; serialising nine agents behind it buys
  nothing.
- **The scope is empty:** the reviewer has to settle it first. Wait for its
  report and hand each lens the scope stated at the top of it. That is data,
  not opinion — passing it breaks no cold-start rule. What must never ride
  along with it is anything the reviewer concluded.

The lenses and their rubrics:

| Lens | Looks for |
|---|---|
| `correctness` | logic errors, off-by-one, wrong operator, null/empty/zero/max edges, races, unhandled promises, missing await |
| `security` | unvalidated input at boundaries, hardcoded secrets, injection, sensitive data in logs and errors, authz gaps |
| `tests` | new branches with no test, uncovered edge cases, tests that cannot fail, flakiness, fixtures that hide the bug |
| `maintainability` | unclear names, functions doing several things, unactionable error messages, comments explaining *what*, changes bundling unrelated concerns |
| `resilience` | outbound calls with no timeout, retries with no backoff or no cap, a failure swallowed into a default that reads as success, multi-step work that leaves inconsistent state when it fails halfway, a retried write that is not idempotent, a call the code assumes cannot fail, a new failure path nothing logs |
| `reuse` | logic the repo already implements elsewhere, a second copy of something within the diff itself, a hand-rolled version of what a dependency already in the manifest provides, a new abstraction where an existing one would have served, code shared between two things that only look alike — and code the change orphaned but did not remove: a function whose last caller went away, a config key nothing reads, a flag now permanently on with its dead branch intact |
| `performance` | N+1 queries, work inside loops that belongs outside, resource leaks, blocking calls in async paths, unbounded growth |
| `dependencies` | new dependencies (necessity, maintenance, transitive weight), breaking changes to public interfaces, config formats or CLI flags, irreversible migrations |

There is deliberately no linter lens. The reviewer already ran the project's real
linter and reported it; a model imitating static analysis is strictly worse than
the tool that does it exactly.

The `tests` lens judges coverage by ROI: a new branch with no test is a finding;
trivial code without one is not.

`reuse` is the one lens whose subject sits outside the diff: both the duplicate it
looks for and the code the change orphaned live in files the change did not touch.
Every finding it writes is therefore an assertion about code nobody in this run
has been asked to read, which sets its evidence bar. Search before asserting an
absence, and count re-exports, string-keyed lookups and dynamic dispatch as
callers. "Something like this probably already exists" and "nothing uses this any
more" are the two shapes this lens fails in, and neither is reportable without the
search behind it. The orphan half is the more dangerous, because a wrong claim
there invites a deletion.

The two halves anchor differently. A duplication finding anchors at the changed
code and cites the existing implementation by `file:line` in the finding itself —
the anchor is the line the reader has to act on, and anchoring at the pre-existing
copy instead would collapse two added duplicates into one finding when the reports
are deduplicated below. An orphan finding anchors at the orphaned code, since that
is the line that gets deleted.

Duplication is also the finding most often worth leaving alone, so it judges by
whether the two copies have to change together, not by how alike they look.

`correctness` and `resilience` are next to each other and must not merge.
`correctness` asks whether the code computes the right answer from the inputs it
was handed; `resilience` asks what happens when something the code *calls* fails,
hangs or half-succeeds. A missing `await` stays with `correctness` — it is wrong
regardless of whether the callee misbehaves.

`performance` and `resilience` divide by path, not by subject. `performance` owns
the happy path — what this costs when it works and the input is large.
`resilience` owns the failure path. A leaked handle belongs to whichever path
leaks it: not closed on the way through is `performance`, skipped because an
exception jumped over the cleanup is `resilience`. A retry that hammers a
struggling dependency is `resilience`; the loop that makes each attempt expensive
is `performance`.

### Synthesise

Merge the eight reports with the reviewer's own. Deduplicate on the underlying
defect, not the exact line — two agents describing the same problem routinely
anchor a few lines apart. Where they found the same thing, keep the more
specific statement and drop the other, rather than listing it twice with
different wording. Where they disagree on tier, take the higher and say both
lenses saw it.

Do not print the merged report yet — it has not been verified, and findings that
are about to be retracted should not get a first airing.

### Verify

Every finding so far was judged by the agent that wrote it, which is the
position it is worst placed to judge from — and under `--deep` there are eight
agents each under quiet pressure to justify their dispatch, which is exactly the
pressure that produces plausible findings that are not real.

Refute the Critical and Warning findings; carry Suggestions through marked
**(unverified)** and say how many went unchecked. Suggestions are the most
numerous tier, and one agent apiece to verify a naming nit is the bulk of the
spend for the least of the value. Never drop one silently to save the spawn — an
unverified finding the reader knows is unverified is honest; one that disappears
is not.

Spawn one `wtf-refuter` subagent per finding being verified, in parallel. It
already knows to argue the code is correct, to re-run a command the finding
claims to have observed, and to default to `refuted` when it cannot decide.

How many that is depends on what the earlier passes found, so it cannot be
announced with the lens count. **Say the number once you know it**, before you
spawn them, and say what it brings the run's total to. The count that scales
with the diff is the one worth disclosing, and it is the one the user has not
already agreed to.

**Send it the finding verbatim, plus the scope, and nothing else.** The scope
rides along for the same reason it rides to the lenses — it is data, not
opinion: a refuter reads the working tree unless told otherwise, so on a scope
that is not checked out it would judge every finding against the wrong files
and kill the real ones. Name the ref or tree the findings are about, and when
that tree is not the user's own work — a fetched PR, a contributor's branch —
say that too, because the refuter's decision to run a cited command depends on
it. What still must not ride along: why you think it might be wrong, where you
would look first, that you already checked something. This is the same rule as
the reviewer dispatch above and it exists for the same reason: if you wrote the
code, a hint about where the refutation lies is you arguing your own case
through an agent spawned to judge it. A refuter you steered has told you
nothing.

### Report

Print the merged report of what survived, in the reviewer's Critical / Warning /
Suggestion format, keeping its Scope / Tests / Lint header lines — the test
result is the most load-bearing line in the report, and under `--deep` this is
its only airing. Then:

- how many findings were refuted, and why — a dropped finding is reported, not
  hidden
- which lenses returned nothing; a silent lens is information, and hiding it
  makes eight agents look like one
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then stop. The same close as above: the findings are the user's to triage, and
fixes happen only if they ask — when they do, follow the next section.

## If the user asks for fixes

Make only the edits the named findings describe — a review is not a mandate to
refactor — and leave committing to the user.

Then have the fixes checked, because of who wrote them. Everything above is
built on the author being the worst-placed judge of their own work, and the
fixes were just written here, in the conversation the reviewer was deliberately
kept out of. The original diff got a cold reviewer; the edits repairing it get
nothing unless you dispatch it.

Spawn one `wtf-refuter` per fixed Critical and Warning finding, in parallel,
with the finding **as the review wrote it** and nothing else — not the fix, not
which lines it touched, not that a fix exists. The finding's `file:line` may
have drifted under the edits; locating the code in the tree as it now stands is
the refuter's job, not a reason to annotate the dispatch. Say how many refuters
that is before spawning them. Re-run the tests the reviewer's report named in
the same batch — neither depends on the other, and serialising the suite behind
the verdicts buys nothing — and report the result alongside them, `not run:
reason` when it cannot happen.

The verdicts read inverted from the verify pass: the refuter argues the code is
correct, so against the fixed tree `refuted` means the problem is gone and
`stands` means the fix did not take. Relay them to the user in fix terms —
**resolved** and **fix did not take**, with the raw verdict in parentheses if
fidelity matters — because a user who asked for fixes and reads "3 refuted"
will hear the fixes failing.

A finding that still stands goes back to the user with the refuter's reasoning
verbatim. Do not quietly take another swing and re-verify — a fix that failed
its check once is a fix a human should look at.

What this check does not cover, so it is not mistaken for more than it is:

- A refuter confirms the finding is resolved, not that the fix broke nothing
  else. The test re-run above is the only check that covers regressions, which
  is why its result — `not run: reason` included — belongs in the report.
- Fixed Suggestions go unverified — the same economics as the verify pass —
  and are reported as such.
- The refuter defaults to `refuted` when the evidence is ambiguous, and that
  default now lands in the fix's favour — relay a verdict whose reasoning looks
  thin as exactly that. A `stands` whose reasoning says the check was blocked
  (the refuter declined to run the decisive command) is not the fix failing:
  relay it as **could not verify**.
- A fix that reached beyond the finding's own hunk has changed code no refuter
  was pointed at. Offer a fresh `/wtf-review-changes` for it instead of
  presenting the verdicts as if they covered it.

## If the findings go to GitHub

A later message may ask for these findings to be posted on a PR — as inline
review comments, as a review body, or as a single comment. When it does, **every
finding carries its tier with it, whether or not the user asked for severities.**
Someone reading a comment on GitHub cannot see the report it came from, so an
unlabelled finding arrives with no way to tell whether it blocks the merge or is
a naming nit.

Lead each posted comment with its tier, then the finding as it was written:

```markdown
**Critical** — Token expiry compared with `>` instead of `>=`, so a token
expiring exactly now is accepted. Use `>=`.
```

Where one comment covers several findings, group them under the same Critical /
Warning / Suggestion headings the report used instead of tagging each line.

Carry the qualifiers across too. **(unverified)** and **(pre-existing)** change
what the reader should do about a finding as much as the tier does, and a
suggestion nothing refuted should not land on the PR looking as settled as one
that survived a refuter.

Do not re-rank on the way out. The tier that gets posted is the tier the review
gave it, including any you would have scored differently.
