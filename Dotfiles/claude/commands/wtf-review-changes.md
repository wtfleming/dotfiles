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
Agent tool, `subagent_type: "wtf-change-reviewer"`, and wait for it to
complete — except under `--deep` with a named scope, where
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

Then, below the report, add the **Suggestion triage** described under
**Triage the Suggestions** — the one place this command adds an opinion of its
own, and it goes after the report rather than into it.

**Without `--deep`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, follow **If the user asks for fixes** below.

## Triage the Suggestions

Suggestions are the most numerous tier and the least sorted: a rename worth
two minutes sits next to a style nit nobody should spend time on, in the same
list, in the same voice. Sorting them is cheap and leaves the user to read only
the ones that matter, so after every report print one more section:

```markdown
## Suggestion triage

**Definitely worth doing**
- `src/api.ts:12` — <one line: what it buys, and why now — the fix is small and the cost of leaving it compounds>

**Worth doing**
- `src/api.ts:40` — <one line: what the suggestion buys>

**Not worth doing**
- `src/util.ts:30` — <one line: why — no reader is confused, style matches the file, churn outweighs the gain>
```

Every Suggestion in the report lands in exactly one list, cited by the same
`file:line` so the lists can be matched to the report, with a one-line reason
each. **Definitely worth doing** is for the few a reader should not skip: the
change is small and the payoff is clear and durable — a misleading name on
something public, dead code that will be mistaken for live, a comment that
states something false. **Worth doing** is the rest of the genuine
improvements — right to take, fine to defer. Keep the top list short; if most
Suggestions land there, it is not sorting anything. This is a
judgement, not a verification: nothing is dispatched to check a Suggestion, and
the triage says so in a closing line. It is also not a licence to drop one —
the report above still carries all of them verbatim.

One shape does not belong in either list. A Suggestion whose content describes
something that *breaks* — a specific input and a wrong result, a leak, an
unhandled failure, a new branch with no test, a perf trap — is a Warning by the
reviewer's own definitions, filed a tier low. Tier follows the content, not the
label the finding arrived with:

- under `--deep`, it is promoted to Warning *before* the verify pass and refuted
  with the rest — see **Verify**. The report lists it under Warning marked
  **(promoted from Suggestion)**, and it does not reappear in the triage.
- without `--deep`, there is no refuter to send it to. Leave it in the report
  as written, and list it in the triage under a third heading,
  **Reads as a Warning (unverified)**, so it is not mistaken for a nit.

Promotion is the one exception to the relaying rule, and it is narrow: a
finding moves only when it states a concrete failure that the Warning
definition covers. "Could be cleaner" does not move. When unsure, do not
promote — a wrongly promoted nit costs a refuter; a wrongly kept one is still
in the triage for the user to see.

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
different wording. Where they disagree on tier, take the higher and say which
reports saw it.

Do not print the merged report yet — it has not been verified, and findings that
are about to be retracted should not get a first airing.

### Verify

Every finding so far was judged by the agent that wrote it, which is the
position it is worst placed to judge from — and under `--deep` there are eight
agents each under quiet pressure to justify their dispatch, which is exactly the
pressure that produces plausible findings that are not real.

First sort the Suggestions for misfiled Warnings, as **Triage the Suggestions**
describes: a Suggestion that states a concrete failure is promoted to Warning
here, before any `wtf-refuter` is spawned, so it gets a refuter rather than an
unverified pass. Say how many moved.

Refute the Critical and Warning findings — promoted ones included; carry the
remaining Suggestions through marked **(unverified)** and say how many went
unchecked. Suggestions are the most
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
and kill the real ones. Name the ref or tree the findings are about, and whose
work it is — stated both ways, because the refuter treats silence as untrusted:
an ordinary review of the user's own branch says so plainly, and a fetched PR
or a contributor's branch is named as such. The refuter's decision to run a
cited command depends on it. When the tree is untrusted, the refuter will not
run a cited command unless the dispatch says the user explicitly sanctioned
that — so relay the sanction when the user has given it, and never otherwise. What still must not ride along: why you think it might be wrong, where you
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
- which findings were promoted from Suggestion to Warning and how each fared
  — a promotion is this command's own re-tiering, so it is disclosed alongside
  the refutations rather than folded into the reviewer's count
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then the **Suggestion triage**, over the Suggestions that remain.

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
with the finding **as the review wrote it**, plus the same scope-and-provenance
data the verify pass sends — here that is the working tree, where the fixes
landed, and whose work it is — and nothing else: not the fix, not which lines
it touched, not that a fix exists. The finding's `file:line` may have drifted
under the edits; locating the code in the tree as it now stands is the
refuter's job, not a reason to annotate the dispatch. Say how many refuters
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

A later message may ask for these findings to be posted on a PR. When it does,
**every finding carries its tier with it, whether or not the user asked for
severities.** Someone reading a comment on GitHub cannot see the report it came
from, so an unlabelled finding arrives with no way to tell whether it blocks
the merge or is a naming nit.

Lead each posted comment with its tier, then the finding as it was written:

```markdown
**Critical** — Token expiry compared with `>` instead of `>=`, so a token
expiring exactly now is accepted. Use `>=`.
```

Carry the qualifiers across too. **(unverified)**, **(pre-existing)** and
**(promoted from Suggestion)** change what the reader should do about a finding
as much as the tier does, and a suggestion nothing refuted should not land on
the PR looking as settled as one that survived a refuter.

Do not re-rank on the way out. The tier that gets posted is the tier the review
gave it, including any you would have scored differently.

### How to post

Default to one inline review comment per finding, anchored at its `file:line`
— it puts each finding where the reader is already looking when they open that
line, and is more GitHub-native than a wall of text. Post them as a single PR
review rather than one API call per comment (a `POST .../pulls/{number}/reviews`
with a `comments` array, or the `gh` equivalent), so they land together as one
review instead of trickling in as separate notifications.

Inline anchoring only works within the PR's diff hunks — GitHub rejects a
comment on a line the diff does not touch. Fetch the PR's actual hunks (`gh pr
diff` or the per-file `patch` from the PR's files) and check each finding's
`file:line` against them *before* posting, rather than discovering the
rejection from a failed call. Anchor each comment with `path` and `line` +
`side` — not the deprecated `position` — and set the review's `commit_id` to
the PR's current head SHA, so a comment doesn't silently land against a stale
commit.

- A finding whose line falls inside a hunk goes up as its own inline comment,
  tier-led as above.
- A finding whose line does not — unchanged context the diff doesn't cover, a
  file touched only indirectly, a `file:line` that drifted — cannot anchor.
  Collect all such findings into the review's body instead, grouped under the
  same Critical / Warning / Suggestion headings the report used, the same way
  a fully-grouped review would be written.
- Say, when posting, how many went inline and how many fell back to the body,
  so the split is visible rather than silently mixed.

If the user asks for a different shape instead — a single review comment for
everything, or inline for everything with no fallback — do that instead; this
default is what to do absent other instructions, not a rule to argue for over
an explicit request.
