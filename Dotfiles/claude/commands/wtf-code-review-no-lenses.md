---
description: Independent code review in a fresh context — recent changes, or a named subject. Diff, tests, lint, structured report. Pass --deep to have the one reviewer cover every dimension, verified. The single-agent variant of /wtf-code-review.
argument-hint: "[ref, branch, path or subject — defaults to uncommitted, else the branch, else HEAD] [--deep]"
allowed-tools: Agent, Read, Grep, Glob
---

Arguments: $ARGUMENTS

Split those into a scope and the optional flag `--deep`. Everything that is not
the flag is the scope, and the scope may be empty.

This is the single-agent variant of `/wtf-code-review`. It is the same command
in every respect but one: where that one spawns a `wtf-lens` per dimension under
`--deep`, this one hands the same rubrics to the one reviewer and has it work
through them itself. The two exist to be compared — same scope, same report
shape, different cost — so nothing else here should drift from that command.

This command reviews; it does not fix. If the user wants findings acted on,
they will say which ones in a later message, and that happens in the main
conversation where their intent lives — not here.

## Review

Launch the `wtf-change-reviewer` subagent on the scope. Dispatch it with the
Agent tool, `subagent_type: "wtf-change-reviewer"`, and wait for it to
complete. Under `--deep` the same dispatch carries the extra rubrics described
under **With `--deep`** below — there is only ever one reviewer, on either
path.

The whole point is that the reviewer starts cold. So the prompt you send it
contains **only** the scope. Do not include:

- your summary of what the change does or why
- which parts you think are fine, or which you are unsure about
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid. Hand over the scope and nothing else. If the scope is empty, say so and
let the agent work out its own.

Without `--deep`, print the report verbatim when it returns — unless it carries
a Critical, in which case hold it and follow **Verify the Criticals** first. Do
not re-rank the findings, soften them, or defend the code — you are relaying an
independent review, not negotiating with it.

Under `--deep`, do not print it yet. Its findings are about to enter a verify
pass that may retract some of them, and a finding that is about to be retracted
should not get a first airing here any more than in the verified report below.
Say that the reviewer returned and how many findings it brought, and hold the
rest. The relaying rule still applies to the report you eventually print.

Then, below the report, add the **Suggestion triage** described under
**Triage the Suggestions** — the one place this command adds an opinion of its
own, and it goes after the report rather than into it. It is also where the
Suggestions themselves are printed: leave the **Suggestion** section out of the
report and let the triage carry them, so each one appears once, in the list
that says what to do about it. That is the only rearrangement allowed — the
findings still go out as written, in the tier they arrived in.

**Without `--deep`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, follow **If the user asks for fixes** below.

## Verify the Criticals

Without `--deep` the reviewer's findings reach the user checked by nobody, and
the bias that objection rests on does not depend on how many agents ran: the
agent that wrote a finding is the worst-placed to judge it, alone or in a crowd.

Verifying all of them here would cost what `--deep` costs on the path chosen for
being cheap — and a refuter cannot ride along with the reviewer, because a
finding has to exist before anything can argue against it. So this path
verifies **Critical findings only**:

- Critical is the tier that claims to block the commit, so a false one is the
  most expensive finding in the report: it stops work that should not stop.
- It is rare. Most runs carry none and spawn nothing, which is what keeps this
  path as fast as the reviewer alone.

Spawn one `wtf-refuter` per Critical, in parallel, dispatched exactly as
**Verify** describes under `--deep` — the finding verbatim, plus the scope and
whose work it is, and nothing else. Say how many before you spawn them.

Then print the report with the refuted Criticals removed, and say how many were
refuted and why; a dropped finding is reported, not hidden. If every Critical
was refuted, say so plainly and treat it as a result worth doubting rather than
a clean bill of health.

Warnings and Suggestions go through unchecked. Mark each Warning
**(unverified)** so no reader mistakes an unchecked finding for a checked one,
and leave the Suggestions to the triage below, which already says it verifies
nothing.

## Triage the Suggestions

Suggestions are the most numerous tier and the least sorted: a rename worth
two minutes sits next to a style nit nobody should spend time on, in the same
list, in the same voice. Sorting them is cheap and leaves the user to read only
the ones that matter, so after every report print one more section — and print
the Suggestions in it rather than in the report, so a reader meets each one
once, already classified:

```markdown
## Suggestion triage

**Definitely worth doing**
- `src/api.ts:12` — <the finding as the reviewer wrote it> — <one line: what it buys, and why now — the fix is small and the cost of leaving it compounds>

**Worth doing**
- `src/api.ts:40` — <the finding as the reviewer wrote it> — <one line: what the suggestion buys>

**Not worth doing**
- `src/util.ts:30` — <the finding as the reviewer wrote it> — <one line: why — no reader is confused, style matches the file, churn outweighs the gain>
```

Every Suggestion lands in exactly one list, carrying its `file:line`, the
finding as written, and any qualifier it arrived with — **(unverified)** under
`--deep` — plus the one-line reason. This is the only place it appears, so a
finding shortened here is shortened everywhere. Findings under **Pre-existing**
are the exception and are not sorted into these lists, whatever tier they
carry — they are tickets, not work for this change, and they stay in that
section of the report, once. The promotion rule below still applies to them:
tier follows content there as anywhere, and the section does not change that.
**Definitely worth doing** is for the few a reader should not skip: the change
is small and the payoff is clear and durable — a misleading name on something
public, dead code that will be mistaken for live, a comment that states
something false. **Worth doing** is the rest of the genuine
improvements — right to take, fine to defer. Keep the top list short; if most
Suggestions land there, it is not sorting anything. This is a
judgement, not a verification: nothing is dispatched to check a Suggestion, and
the triage says so in a closing line. It is also not a licence to drop one:
nothing above carries a Suggestion except the two cases named here — a
Pre-existing one, and one promoted to Warning — so every other one the reviewer
wrote has to be here.

One shape does not belong in either list. A Suggestion whose content describes
something that *breaks* — a specific input and a wrong result, a leak, an
unhandled failure, a new branch with no test, a perf trap — is a Warning by the
reviewer's own definitions, filed a tier low. Tier follows the content, not the
label the finding arrived with:

- under `--deep`, it is promoted to Warning *before* the verify pass and refuted
  with the rest — see **Verify**. The report lists it under Warning marked
  **(promoted from Suggestion)**, and it does not reappear in the triage. A
  Pre-existing one stays in its section with the new tier leading it, marked
  the same way.
- without `--deep`, there is no refuter to send it to: that path verifies
  Criticals only, and a promotion never reaches Critical. Print it in the triage
  under a third heading, **Reads as a Warning (unverified)**, as the reviewer
  wrote it, so it is not mistaken for a nit. A Pre-existing one is the exception
  again: it stays in its section with the new tier leading it, marked
  **(promoted from Suggestion, unverified)** — the bullet above renders a
  promotion a refuter survived, and nothing checked this one — and does not
  appear in the triage.

Promotion is the one exception to the relaying rule, and it is narrow: a
finding moves only when it states a concrete failure that the Warning
definition covers. "Could be cleaner" does not move. When unsure, do not
promote — a wrongly promoted nit costs a refuter; a wrongly kept one is still
in the triage for the user to see.

## With `--deep`

One reviewer covering six dimensions gives some of them a shallower pass than
the others, and two dimensions — `reuse` and `resilience` — its checklist does
not cover at all. `/wtf-code-review` answers that with an agent per dimension.
This command answers it with a rubric per dimension, handed to the one reviewer
already reading the scope, because the reading is the part that gets paid for
twice: eight lenses each diff the change and read the touched files in full
before any of them writes a line.

So there is no second wave of agents here. `--deep` changes one thing: what the
reviewer is asked to look for. Say that up front — one reviewer, and refuters
later whose count is not knowable yet — so the user knows what this run costs
before it is spent.

Dispatch the reviewer exactly as **Review** describes, with the scope and the
block below appended, and nothing else. The rubrics are the same data every run
sends; what must still never ride along is anything about *this* change — your
summary of it, which parts you think are fine, any reasoning from this
conversation. The cold-start rule is unchanged.

Send this block verbatim:

````markdown
Cover these dimensions as well as your checklist, in one pass, over the scope
you settle on. This replaces nothing in your instructions — the tests, the
linter, the checklist and the report format all still apply. It widens what you
are hunting for, and it asks you to account for each dimension explicitly.

| Dimension | Looks for |
|---|---|
| `correctness` | logic errors, off-by-one, wrong operator, null/empty/zero/max edges, races, unhandled promises, missing await |
| `security` | unvalidated input at boundaries, hardcoded secrets, injection, sensitive data in logs and errors, authz gaps |
| `tests` | new branches with no test, uncovered edge cases, tests that cannot fail, flakiness, fixtures that hide the bug |
| `maintainability` | unclear names, functions doing several things, unactionable error messages, comments explaining *what*, changes bundling unrelated concerns |
| `resilience` | outbound calls with no timeout, retries with no backoff or no cap, a failure swallowed into a default that reads as success, multi-step work that leaves inconsistent state when it fails halfway, a retried write that is not idempotent, a call the code assumes cannot fail, a new failure path nothing logs |
| `reuse` | logic the repo already implements elsewhere, a second copy of something within the diff itself, a hand-rolled version of what a dependency already in the manifest provides, a new abstraction where an existing one would have served, code shared between two things that only look alike — and code the change orphaned but did not remove: a function whose last caller went away, a config key nothing reads, a flag now permanently on with its dead branch intact |
| `performance` | N+1 queries, work inside loops that belongs outside, resource leaks, blocking calls in async paths, unbounded growth |
| `dependencies` | new dependencies (necessity, maintenance, transitive weight), breaking changes to public interfaces, config formats or CLI flags, irreversible migrations |

There is deliberately no linter dimension. You already ran the project's real
linter; imitating static analysis is strictly worse than the tool that does it
exactly.

Some of these have no surface on some scopes, and saying so is part of the
answer. For each dimension, land on one of three:

- **not applicable** — there is nothing here this dimension governs. A
  `dependencies` pass over a change that adds no import, alters nothing a caller
  depends on — an exported signature, a config key, a CLI command name or
  flag — and adds no migration has nothing to govern. Reach this from the files
  you read, never from what the scope is called.
- **no findings** — it governs something here, and it is clean.
- findings.

`tests`, `reuse`, `resilience` and `security` cannot take the **not applicable**
exit because the thing they hunt is absent — for those four that absence *is*
the finding. They are not applicable only when the scope holds no code they
could govern at all.

What counts as code, since that rule turns on it: prose is an allowlist, not a
judgement — `.md`, `.markdown`, `.rst`, `.adoc`, `.txt`, and the extensionless
`README`, `LICENSE`, `CHANGELOG` and `NOTICE`. Anything else — config, a script,
an extension not listed, a file with none — is code. So is a Markdown file that
is an agent's instructions: `CLAUDE.md`, `AGENTS.md`, `SKILL.md`, or anything
under a `claude/`, `.claude/`, `agents/`, `commands/` or `skills/` directory.
Those are executed, and a change to one is a change to what an agent does.

`correctness`, `security`, `maintainability` and `reuse` still run on prose: a
doc can state something false, leak a secret, or duplicate a passage that now
has to change in step with the original.

Three boundaries, because they are the ones that blur:

- `correctness` asks whether the code computes the right answer from the inputs
  it was handed; `resilience` asks what happens when something the code *calls*
  fails, hangs or half-succeeds. A missing `await` is `correctness` — it is
  wrong regardless of whether the callee misbehaves.
- `performance` owns the happy path — what this costs when it works and the
  input is large. `resilience` owns the failure path. A leaked handle belongs to
  whichever path leaks it: not closed on the way through is `performance`,
  skipped because an exception jumped over the cleanup is `resilience`.
- `tests` judges coverage by ROI: a new branch with no test is a finding;
  trivial code without one is not.

`reuse` is the one dimension whose target sits outside the diff: both the
duplicate it looks for and the code the change orphaned live in files the change
did not touch. Every finding there is an assertion about code nobody has been
asked to read, which sets its evidence bar. Search before asserting an absence,
and count re-exports, string-keyed lookups and dynamic dispatch as callers.
"Something like this probably already exists" and "nothing uses this any more"
are the two shapes this fails in, and neither is reportable without the search
behind it. The orphan half is the more dangerous, because a wrong claim there
invites a deletion. A duplication finding anchors at the changed code and cites
the existing implementation by `file:line` in the finding itself; an orphan
finding anchors at the orphaned code, since that is the line that gets deleted.
Duplication is also the finding most often worth leaving alone, so judge it by
whether the two copies have to change together, not by how alike they look.

Spend real attention here rather than a sweep per row: read whole files rather
than hunks, follow the call sites out of the diff, and check the case the author
probably did not. A dimension with nothing to report is a real and useful
answer — do not manufacture a finding to fill a row. The bar for writing one is
the bar you already have: try to refute it first, and drop it unless you can
state a concrete failure.

Add one section to the end of your report, after the closing line:

```markdown
## Dimensions
- `correctness` — 2 findings
- `dependencies` — not applicable: no import, exported signature, config key,
  CLI flag or migration changed
- `performance` — no findings
```

Every dimension in the table appears in that list exactly once, with its finding
count, `no findings`, or `not applicable: <what you looked for>`. A reader
judging how far the pass reached has to be able to tell a dimension that came
back clean from one that had no surface, and neither is recoverable from the
findings list.
````

The reviewer settles applicability from the files it has read, so there is no
listing step here and no dimension to skip before dispatch — the check
`/wtf-code-review` runs before spawning exists to save spawns, and there are
none to save. A subject scope needs no second round either: nothing downstream
is waiting on the reviewer to settle the file list, so every scope shape runs as
one dispatch.

If the reviewer comes back saying nothing in the repo implements the subject, it
stopped at step 1: there is no report and no Scope / Tests / Lint lines. Relay
what it said and stop — do not synthesise a report around a header you cannot
fill.

It returns one report, so there is nothing to merge, nothing to deduplicate
and no tiers to reconcile. Do not print it yet — it has not been verified, and findings
that are about to be retracted should not get a first airing.

### Verify

Every finding so far was judged by the agent that wrote it, which is the
position it is worst placed to judge from — and under `--deep` that agent was
handed eight rubrics and asked to account for each one, which is quiet pressure
to fill a row, which is exactly the pressure that produces plausible findings
that are not real.

First sort the Suggestions for misfiled Warnings, as **Triage the Suggestions**
describes: a Suggestion that states a concrete failure is promoted to Warning
here, before any `wtf-refuter` is spawned, so it gets a refuter rather than an
unverified pass. Say how many moved.

Refute the Critical and Warning findings — promoted ones included, and
Pre-existing ones at those tiers, since a ticket for a bug that is not there
costs as much as a fix for one; carry the remaining Suggestions through marked
**(unverified)** and say how many went unchecked. Suggestions are the most
numerous tier, and one agent apiece to verify a naming nit is the bulk of the
spend for the least of the value. Never drop one silently to save the spawn — an
unverified finding the reader knows is unverified is honest; one that disappears
is not.

Spawn one `wtf-refuter` subagent per finding being verified, in parallel. It
already knows to argue the code is correct, to re-run a command the finding
claims to have observed, and to default to `refuted` when it cannot decide.

How many that is depends on what the reviewer found, so it could not be
announced when the run was. **Say the number once you know it**, before you
spawn them, and say what it brings the run's total to. This is the only count
in the run that scales with the diff, which makes it the one worth
disclosing.

**Send it the finding verbatim, plus the scope, and nothing else.** The scope
rides along for the same reason it rides to the reviewer — it is data, not
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

Print the report of what survived, in the reviewer's Critical / Warning /
Pre-existing format, keeping its Scope / Tests / Lint header lines — the test
result is the most load-bearing line in the report, and under `--deep` this is
its only airing. The surviving Suggestions print once, in the triage below.
Then:

- how many findings were refuted, and why — a dropped finding is reported, not
  hidden
- the reviewer's **Dimensions** section, verbatim — every entry as written, in
  the order it wrote them, `not applicable` ones keeping what they said they
  looked for. Do not regroup it into clean and not-applicable lists: a dimension
  that governed something and found it clean and one that had no surface to
  review are two different facts about how far the pass reached, and the
  reviewer already distinguishes them entry by entry. If the reviewer's report
  has no Dimensions section, or the section omits dimensions the table names,
  say which are missing rather than filling them in — an unaccounted dimension
  is not a clean one, and writing `no findings` on the reviewer's behalf is this
  command inventing coverage it did not get
- which findings were promoted from Suggestion to Warning and how each fared
  — a promotion is this command's own re-tiering, so it is disclosed alongside
  the refutations rather than folded into the reviewer's count
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then the **Suggestion triage**, carrying the Suggestions that remain, each
marked **(unverified)**.

Then stop. The same close as above: the findings are the user's to triage, and
fixes happen only if they ask — when they do, follow the next section.

## If the user asks for fixes

Make only the edits the named findings describe — a review is not a mandate to
refactor — and leave committing to the user. "Fix everything" means the three
tiers; a **Pre-existing** finding is fixed only if the user names it, because
it belongs to a ticket, not to this branch.

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
  was pointed at. Offer a fresh `/wtf-code-review-no-lenses` for it instead of
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

Carry the qualifiers across too. **(unverified)** and
**(promoted from Suggestion)** change what the reader should do about a finding
as much as the tier does, and a suggestion nothing refuted should not land on
the PR looking as settled as one that survived a refuter. A **Pre-existing**
finding posts as its tier followed by **(pre-existing)** —
`**Warning (pre-existing)** — …` — because the section heading that said so
does not travel with it. The triage's **Reads as a Warning (unverified)**
heading is the same shape of problem and takes the same answer: it posts as
`**Suggestion (reads as a Warning, unverified)** — …`, the heading carried as a
qualifier. Posting it as a Warning instead would be re-ranking, which the next
line forbids.

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
commit. A finding's `file:line` always names code that still exists in the
tree being reviewed, never a deleted line, so `side` is always `RIGHT`.

- A finding whose line falls inside a hunk goes up as its own inline comment,
  tier-led as above.
- A finding whose line does not — unchanged context the diff doesn't cover, a
  file touched only indirectly, a `file:line` that drifted — cannot anchor.
  Collect all such findings into the review's body instead, grouped under
  Critical / Warning / Suggestion / Pre-existing headings, the same way a
  fully-grouped review would be written — the Suggestions taken from the
  triage, which is where they were printed.
- Say, when posting, how many went inline and how many fell back to the body,
  so the split is visible rather than silently mixed.

If the user asks for a different shape instead — a single review comment for
everything, or inline for everything with no fallback — do that instead; this
default is what to do absent other instructions, not a rule to argue for over
an explicit request.
