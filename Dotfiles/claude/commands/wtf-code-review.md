---
description: Independent code review in a fresh context — recent changes, or a named subject. Diff, tests, lint, structured report. Runs a verified parallel pass per dimension; pass --lite for the single-reviewer version.
argument-hint: "[ref, branch, path or subject — defaults to uncommitted, else the branch, else HEAD] [--lite]"
allowed-tools: Agent, Read, Grep, Glob, Bash(git:*), Bash(~/.claude/scripts/resolve-scope.sh:*)
---

Arguments: $ARGUMENTS

Split those into a scope and the optional flag `--lite`. Everything that is not
the flag is the scope, and the scope may be empty.

The full pass is the default: the reviewer, a dedicated agent per dimension, and
a refuter per finding that survives to verification. `--lite` is the reviewer
alone, with only its Criticals verified. Where the text below says *under
`--lite`* it means that cheaper path; everything else describes the default.

`--deep` was the old name for what is now the default. If it arrives, say the
flag is gone and run the default rather than treating it as a scope — a scope of
`--deep` reviews a path that does not exist.

This command reviews; it does not fix. If the user wants findings acted on,
they will say which ones in a later message, and that happens in the main
conversation where their intent lives — not here.

## Review

Launch the `wtf-change-reviewer` subagent on the scope. Dispatch it with the
Agent tool, `subagent_type: "wtf-change-reviewer"`, and wait for it to
complete — except where you resolved the scope before dispatch, which is every
shape but a subject: a named revision and a bare invocation alike, since the
lenses launch in that same batch and cannot wait for the reviewer. Hold the
dispatch for the batch there. Under `--lite` there is no batch to hold it for,
so always wait.

The whole point is that the reviewer starts cold. So the prompt you send it
contains **only** the scope. Do not include:

- your summary of what the change does or why
- which parts you think are fine, or which you are unsure about
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid. Hand over the scope and nothing else. If the scope is empty, say so and
let the agent work out its own — it runs the same resolver you would, so under
`--lite` there is nothing to gain by resolving first. Otherwise you resolve
before dispatch instead, because the lenses launch in the same batch and cannot
wait for it; hand the reviewer the artifact directory there, which is data about
where the code lives and no more a cold-start violation than the scope string is.

Under `--lite`, print the report verbatim when it returns — unless it carries
a Critical, in which case hold it and follow **Verify the Criticals** first. Do
not re-rank the findings, soften them, or defend the code — you are relaying an
independent review, not negotiating with it.

Otherwise, do not print it yet. Its findings are about to enter a verify
pass that may retract some of them, and a finding that is about to be retracted
should not get a first airing here any more than in the merged report below.
Say that the reviewer returned and how many findings it brought, and hold the
rest. The relaying rule still applies to the report you eventually print.

Then, below the report, add the **Suggestion triage** described under
**Triage the Suggestions** — the one place this command adds an opinion of its
own, and it goes after the report rather than into it. It is also where the
Suggestions themselves are printed: leave the **Suggestion** section out of the
report and let the triage carry them, so each one appears once, in the list
that says what to do about it — or, if it is judged not worth doing, in the
count of the ones dropped. That is the only rearrangement allowed — the
findings still go out as written, in the tier they arrived in.

**Under `--lite`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, follow **If the user asks for fixes** below.

## Verify the Criticals

Under `--lite` the reviewer's findings reach the user checked by nobody, and
the bias that objection rests on does not depend on how many agents ran: the
agent that wrote a finding is the worst-placed to judge it, alone or in a crowd.

Verifying all of them here would cost what the full pass costs, on the path
chosen for being cheap — and unlike a lens, a refuter cannot ride along in the
reviewer's batch, because a finding has to exist before anything can argue
against it. So this path verifies **Critical findings only**:

- Critical is the tier that claims to block the commit, so a false one is the
  most expensive finding in the report: it stops work that should not stop.
- It is rare. Most runs carry none and spawn nothing, which is what keeps this
  path as fast as the reviewer alone.

Spawn one `wtf-refuter` per Critical, in parallel, dispatched exactly as
**Verify** describes on the full pass — the finding verbatim, plus the scope and
whose work it is, and nothing else. Say how many before you spawn them.

Then print the report with the refuted Criticals removed, and say how many were
refuted and why; a dropped finding is reported, not hidden. If every Critical
was refuted, say so plainly and treat it as a result worth doubting rather than
a clean bill of health.

Warnings and Suggestions go through unchecked. Mark each Warning
**(unverified)** so no reader mistakes an unchecked finding for a checked one,
and leave the Suggestions to the triage below, which on this path verifies
nothing and says so.

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

_3 Suggestions judged not worth doing and dropped._
```

Every Suggestion is judged against all three of those, and the third is not
printed: a nit nobody should act on costs a reader the same attention as one
they should, and removing that cost is what the sorting is for. It leaves as a
count, not silently — a dropped finding is reported, not hidden — and one line
saying how many is the whole of it. Match that line to the number —
`_1 Suggestion judged not worth doing and dropped._` — and omit it altogether
when nothing was dropped, since a line reporting zero dropped findings reports
nothing. Each Suggestion that *is* printed lands in exactly one list, carrying
its `file:line`, the finding as written, any qualifier it arrived with, and the
one-line reason. The qualifier tracks what checked the finding, not which list
it landed in: mark it **(unverified)** unless a refuter read it and let it
stand, which happens only to the **Definitely worth doing** list on the full
pass. Everything else carries the mark — both lists under `--lite`, and
**Worth doing** on the full pass. So a bare Suggestion means one thing
everywhere, including on a PR comment, where the triage's closing line does not
travel with it. That is the only place it appears, so a finding shortened here
is shortened everywhere. Findings under **Pre-existing** are the exception and are
not sorted into these lists, whatever tier they carry — they are tickets, not
work for this change, and they stay in that section of the report, once. The
promotion rule below still applies to them: tier follows content there as
anywhere, and the section does not change that.
**Definitely worth doing** is for the few a reader should not skip: the change
is small and the payoff is clear and durable — a misleading name on something
public, dead code that will be mistaken for live, a comment that states
something false. **Worth doing** is the rest of the genuine
improvements — right to take, fine to defer. Keep the top list short; if most
Suggestions land there, it is not sorting anything — and on the full pass it is
also spending a refuter on each one, which is the second reason to keep it
short.

Whether the triage verifies anything depends on the path, and its closing line
says which. Under `--lite` it is a judgement and nothing else: nothing is
dispatched to check a Suggestion. Otherwise the **Definitely worth doing**
list is refuted alongside the Criticals and Warnings — see **Verify** — and only
the **Worth doing** list goes out unchecked. The sorting is what makes that
affordable: it is the difference between verifying the tier and verifying the
few findings the triage is actively steering the reader towards.

The third list is the only place the triage itself may leave a Suggestion
unprinted: nothing above carries one except the two cases named here — a
Pre-existing one, and one promoted to Warning. On the full pass there is one
further route out, and it is not the triage's: a refuter kills the finding, and
it leaves with the other refuted findings, counted in that line rather than
this one. Every other Suggestion the reviewer wrote is either in a list or in
the dropped count.

One shape does not belong in either list. A Suggestion whose content describes
something that *breaks* — a specific input and a wrong result, a leak, an
unhandled failure, a new branch with no test, a perf trap — is a Warning by the
reviewer's own definitions, filed a tier low. Tier follows the content, not the
label the finding arrived with:

- on the full pass, it is promoted to Warning *before* the verify pass and
  refuted with the rest — see **Verify**. The report lists it under Warning marked
  **(promoted from Suggestion)**, and it does not reappear in the triage. A
  Pre-existing one stays in its section with the new tier leading it, marked
  the same way.
- under `--lite`, there is no refuter to send it to: that path verifies
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

## The per-dimension pass

One reviewer covering six dimensions gives some of them a shallower pass than
the others. This adds a dedicated pass per dimension, over the same scope the
reviewer reads — plus `reuse` and `resilience`, which the reviewer's checklist
does not cover at all.

**`--lite` skips this section and the rest of the per-dimension pass**, stopping
where **Review** says to. It skips the *pass*, not the file: **If the user asks
for fixes** is where **Review** sends a `--lite` run when the user asks, and
**If the findings go to GitHub** governs findings from either path. Both sit
below this section and both apply. It exists for the runs where the reviewer
alone is the right spend — a one-file change, a second look at something already
reviewed — and it is a deliberate choice, not the fallback for a scope that
merely looks small.

Say up front how many agents you are about to spawn, so the cost is the user's
to refuse before it is spent rather than after. Not every scope has a surface
for every lens, so the number comes from **Pick the lenses** below, which reads
the manifest resolved above — and the announcement names the lenses being
skipped alongside the ones being launched.

Before anything is spawned, resolve the scope once:

```sh
~/.claude/scripts/resolve-scope.sh resolve [--scope <the user's scope>]
```

It prints the scope line and an artifact directory holding `scope.diff` and
`manifest.json`. **Exit 2 means the scope is a subject** — prose naming an area of
behaviour, which has no diff — and sends you to the second branch below. Any other
non-zero exit is a real failure: report it and stop rather than reviewing something
else.

Dispatch one `wtf-lens` subagent per lens that survives that check, **in
parallel**, each with the scope and its own rubric and nothing else. Unlike the
reviewer, a lens cannot derive its own scope, and several agents each guessing
one is how "the same scope" stops being true — which is why the scope travels as
the **artifact directory**, not as a description. Handing over a path is data
about where the code lives, exactly like the file list and label below, and
breaks no cold-start rule. Where it comes from depends on what the user gave:

- **The user named a revision — a ref, a branch, a path — or named nothing at
  all:** the script has already settled it, so every lens gets the artifact
  directory and the manifest's `correspondence`, and nothing a lens does depends
  on the reviewer's output — so launch them all *alongside* the reviewer, in the
  same batch, rather than after it. The reviewer's test run is the long pole of
  the whole pass; serialising the lenses behind it buys nothing. Naming nothing
  used to force a second round because only the reviewer could resolve it; it no
  longer does, and that is the cheapest round this command saves.
- **The user named a subject:** there is no diff to resolve — the script exits 2
  saying so — and the reviewer has to settle it first. Wait for its report and
  hand each lens the *file list* it settled on, labelled as a subject and
  carrying the subject line with it. The list is what the lens reads; the label
  is what stops it diffing. A bare file list is path-shaped, and a lens handed a
  path diffs the working tree for it — which on a subject is empty, so the pass
  reads nothing and marks whatever it does find pre-existing. The list and the
  label are both data, not opinion, and passing them breaks no cold-start rule.
  What must never ride along is anything the reviewer concluded.

A subject belongs in the second branch for the reason the paragraph above
opens with: prose is not something a lens can pin files with, so eight lenses
each resolving it on their own is precisely the eight-guesses failure, and the
merged Scope line would then name a file set that several findings did not come
from. It costs the batched launch — say so when you announce the agents, since
a subject is now the **only** shape where the pass runs in two rounds rather
than one.

That ordering also settles what happens when the reviewer finds nothing: if it
comes back saying nothing in the repo implements the subject, it stopped at step
1, so there is no report and no Scope / Tests / Lint lines. No lens has been
dispatched yet, and none should be. Relay what the reviewer said, say that the
lenses were not launched, and stop — do not synthesise a report around a header
you cannot fill.

### Pick the lenses

A lens with no surface still costs a dispatch: it reads the whole scope before
it can say **not applicable**. One absence is visible from the file list alone —
a change that touches no code — and it can be skipped before the spawn.

**The file list is `manifest.files`**, already resolved. Do not run a second set
of git commands to build one: the manifest's list is projected from `scope.diff`
itself, so it cannot disagree with the diff the lenses are about to read, and a
list derived separately can. It already includes untracked files, which
`git diff` never lists and which are exactly the change that would otherwise
pass as prose.

Check `manifest.file_list_source` first. `fallback-headers` means the list was
scraped from the diff's headers rather than parsed, so it may be missing files —
and a listing missing a code file is exactly what makes a change read as
prose-only and skip four lenses. Dispatch all eight there, and say why.

A subject has no diff to list, so skip the check and dispatch all eight, and say
so.

The manifest cannot describe an empty scope — the script falls through or exits
rather than writing one, and records why in `fell_through`. So an empty listing
here means something went wrong rather than that every file is prose: dispatch
all eight and say so.

Skip `tests`, `resilience`, `performance` and `dependencies` when every path
in the listing is prose, and say so. Prose is an allowlist, not a judgement:
`.md`, `.markdown`, `.rst`, `.adoc`, `.txt`, and the extensionless `README`,
`LICENSE`, `CHANGELOG` and `NOTICE`. Anything else — config, a script, an
extension not listed, a file with none — is code. So is a Markdown file that is
an agent's instructions: `CLAUDE.md`, `AGENTS.md`, `SKILL.md`, or anything
under a `claude/`, `.claude/`, `agents/`, `commands/` or `skills/` directory.
Those are executed, and a change to one is a change to what an agent does.

`correctness`, `security`, `maintainability` and `reuse` still run on prose: a
doc can state something false, leak a secret, or duplicate a passage that now
has to change in step with the original.

No other lens has a mechanical skip. `dependencies` is the tempting one — no
manifest changed, so nothing to govern — but half its rubric is breaking
changes to exported signatures, config keys and CLI flags, and a purely
additive diff can make a flag required with no manifest, import or deletion in
sight. Nothing in a file listing clears that, so on a code diff it runs, and
takes its own **not applicable** exit if there is nothing there.

This is a check on the listing, not a reading of the change. Do not open the
diff to decide, and never skip a lens because the change *looks* like it has
nothing for it — if you wrote the code, that is the author waving off a
reviewer, and the pass exists to stop exactly that. When the listing leaves it
unclear, dispatch: a lens that read a thin surface costs a line where one that
was never sent costs the finding.

The lenses and their rubrics:

| Lens | Looks for |
|---|---|
| `correctness` | logic errors, off-by-one, wrong operator, null/empty/zero/max edges, races, unhandled promises, missing await |
| `security` | unvalidated input at boundaries, hardcoded secrets, injection, sensitive data in logs and errors, authz gaps |
| `tests` | new branches with no test, uncovered edge cases, tests that cannot fail, flakiness, fixtures that hide the bug, an invariant a handful of examples cannot pin where the repo's tests already use a property-based harness |
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

That row's property-based clause is gated twice, and both gates carry weight. The
code has to state an invariant a handful of examples cannot pin — a round trip, an
idempotent operation, a comparator, an invariant a mutation must preserve, a
hand-rolled parser or normaliser over a large input domain — and the repo's own
tests have to already use a property-based harness: a generator-driven test that
exists, not a dependency in a manifest. Without the first, "this could have
properties" is true of nearly every function and the lens writes a Suggestion on
every diff. Without the second the finding is a proposal to adopt a dependency and
a testing style, which is `dependencies`' business and far larger than anything a
review Suggestion should carry. Where both hold it is a Suggestion, anchored at the
test file.

`reuse` is the one lens whose target sits outside the diff: both the duplicate it
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

Merge the lens reports with the reviewer's own. Every finding arrives anchored at
a repo-relative `path:line`, so the first pass is mechanical: findings sharing an
anchor are candidates for one defect. Findings with a file but no line match on
path alone. A finding with neither is never merged automatically.

That is a first pass, not the whole job. Deduplicate on the underlying **defect**,
not the exact line — two agents describing the same problem routinely anchor a few
lines apart, so a shared anchor is evidence of a duplicate and a differing anchor
is not evidence against one.

**Tag each merged finding with where it came from** — the lens name, or `reviewer`:

```
- **Critical** · `src/auth.ts:42` · [correctness] — what breaks, and the fix.
```

You already hold each report separately, so no agent has to be asked for this. It
is what lets a reader see which lens earned its dispatch, and what the disposition
list below refers to.

**When two findings collide, work down this ladder and stop at the first rung that
separates them:**

1. **Pre-existing wins over a tier.** A lens marks a problem the change did not
   cause **(pre-existing)** inline; in the merged report it goes under the
   reviewer's **Pre-existing** section with that tier leading it, and not under
   the tier itself. If any report filed it both ways, Pre-existing wins.
2. **The higher tier wins,** and say which reports saw it.
3. **The statement naming a concrete failing input or code path wins** over one
   describing a category of problem.
4. **The reviewer's statement wins over a lens's.** It read the surrounding files
   and ran the tests; a lens read one rubric.
5. **The longer evidence wins.**

The ladder exists because "keep the more specific statement" leaves two
equally-tiered findings with nothing to separate them, and the merging model then
picks by feel — which is exactly the judgement it is worst placed to make, since
the reports it is choosing between were written by its own agents.

**Record a disposition for every candidate finding: `kept`, `merged` or
`dropped`,** with the source it came from and a one-line reason. This is the one
place in this command where a finding could disappear without the reader being
told, and the rule everywhere else here is that a dropped finding is reported, not
hidden. Two lenses raising one defect, collapsed silently, hides both which lens
found it and that the collapse happened at all.

Do not print the merged report yet — it has not been verified, and findings that
are about to be retracted should not get a first airing.

### Verify

Every finding so far was judged by the agent that wrote it, which is the
position it is worst placed to judge from — and here there are up to eight
agents each under quiet pressure to justify their dispatch, which is exactly the
pressure that produces plausible findings that are not real.

First run **Triage the Suggestions** in full, before any `wtf-refuter` is
spawned — both halves of it, because both decide what gets verified. The
promotion moves a Suggestion that states a concrete failure up to Warning, so it
gets a refuter rather than an unverified pass. The sort then splits what is left
into **Definitely worth doing**, **Worth doing** and the dropped count, and the
first of those is verified too. Say how many were promoted and how many landed
in the top list.

Refute the Critical and Warning findings — promoted ones included, and
Pre-existing ones at those tiers, since a ticket for a bug that is not there
costs as much as a fix for one — plus the **Definitely worth doing**
Suggestions. Those last are the ones the triage has just told the reader not to
skip, and an unchecked finding a reader is being steered towards is the one that
costs most when it is wrong. Carry the **Worth doing** Suggestions through
marked **(unverified)** and say how many went unchecked: they are the bulk of
the tier, and one agent apiece to verify a naming nit is most of the spend for
least of the value. Never drop one silently to save the spawn — an unverified
finding the reader knows is unverified is honest; one that disappears is not.

A dropped Suggestion gets no refuter either. The triage judged it not worth
acting on, so a verdict on it changes nothing; it stays a count.

A **Definitely worth doing** Suggestion a refuter kills leaves the report with
the other refuted findings and is counted in the same line — it is not demoted
to **Worth doing**. Refuted means the problem was not there, which is not a
reason to do it later.

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
and kill the real ones. **Send the manifest's `correspondence` and `scope_head`
with it, and the artifact directory too**, which is what turns that from a hope
into an instruction: on anything but `workspace` or `same` the refuter reads the
scope's blobs, and knows that a line it cannot find is not a refutation. The
directory is what makes the fallback reachable — a deleted file is not at
`scope_head` and on `unknown` the head may not be local at all, so `scope.diff`
is the only copy, and a refuter that was never handed it has an instruction it
cannot follow. Name the ref or tree the findings
are about, and whose work it is — stated both ways, because the refuter treats silence as untrusted:
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
Pre-existing format, keeping its Scope / Tests / Lint header lines — the test
result is the most load-bearing line in the report, and on the full pass this
is its only airing. The surviving Suggestions print once, in the triage below.

The **Scope** line is the manifest's `scope_line`, which already carries the
correspondence. Say it even when it is `same`: a reader cannot tell "the tree
holds the reviewed code" from "nobody checked" unless the report distinguishes
them, and every finding below was read out of one tree or the other. Where
`base_stale` is set, say that too — the scope may be wider than the branch.

Then:

- how many findings were refuted, and why — a dropped finding is reported, not
  hidden
- **what the merge did to findings that are not in the report above**: which were
  merged into which, and which were dropped, each with the lens that raised it
  and the reason. Omit the section entirely when nothing was merged or dropped,
  the same way the triage omits a line reporting zero. This is the merge's half
  of the same rule as the line above it — a finding two lenses found and one
  report shows is a fact about the pass, not noise to tidy away
- which lenses returned nothing, and — listed separately — which returned **not
  applicable**, and — listed separately again — which returned **no usable
  report** because they errored, timed out or came back unparseable. A lens that
  governed something and found it clean, a lens that had no surface to review,
  and a lens that never reported are three different facts about how far the
  pass reached, and collapsing any of them into another overstates coverage. The
  third is the one that most looks like the first: an agent that failed is not a
  dimension that came back clean, and counting it as one is how a broken pass
  reads as a passing one. All three are worth printing, and hiding any of
  them makes the lenses look like one. Then — separately again — which
  lenses were **not dispatched** by **Pick the lenses**, each with the check
  that excluded it, so a reader can tell a lens this command skipped from one
  that looked and found no surface. And, should it happen, which returned
  **subject not found**: on a subject the lenses are handed the file list the
  reviewer settled, so a lens that still could not find the subject disagreed
  with the reviewer about what implements it. Print it as its own line, never
  as no findings — that lens reviewed nothing
- which findings were promoted from Suggestion to Warning and how each fared
  — a promotion is this command's own re-tiering, so it is disclosed alongside
  the refutations rather than folded into the reviewer's count
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then the **Suggestion triage**, carrying the Suggestions that remain: the
**Definitely worth doing** list as it came back from the refuters, and the
**Worth doing** list marked **(unverified)**.

Then stop. The same close as above: the findings are the user's to triage, and
fixes happen only if they ask — when they do, follow the next section.

## If the user asks for fixes

Make only the edits the named findings describe — a review is not a mandate to
refactor — and leave committing to the user. "Fix everything" means the three
tiers; a **Pre-existing** finding is fixed only if the user names it, because
it belongs to a ticket, not to this branch.

"Fix the suggestions" means both printed lists — **Definitely worth doing** and
**Worth doing** — not just the top one. The ones counted as not worth doing
stay unfixed: the triage judged the churn to outweigh the gain, and asking for
the suggestions is not asking to reverse that.

Then have the fixes checked, because of who wrote them. Everything above is
built on the author being the worst-placed judge of their own work, and the
fixes were just written here, in the conversation the reviewer was deliberately
kept out of. The original diff got a cold reviewer; the edits repairing it get
nothing unless you dispatch it.

Spawn one `wtf-refuter`, in parallel, per fixed Critical and Warning finding,
per fixed **Definitely worth doing** Suggestion, and per fixed finding under the
triage's **Reads as a Warning (unverified)** heading — that heading exists
because the content is a Warning, so a fix to one is checked as a Warning's is.
Send the finding **as the review wrote it**, plus the same scope-and-provenance
data the verify pass sends — here that is the working tree, where the fixes
landed, and whose work it is, so the correspondence you send is `workspace` — and
nothing else: not the fix, not which lines
it touched, not that a fix exists. The finding's `file:line` may have drifted
under the edits; locating the code in the tree as it now stands is the
refuter's job, not a reason to annotate the dispatch. Say how many refuters
that is before spawning them.

Re-run the tests **and the linter** the reviewer's report named, in the same
batch — neither depends on the verdicts, and serialising them behind it buys
nothing — and report both results alongside them, `not run: reason` when one
cannot happen. The linter is here for what the Suggestion fixes tend to be: an
import left unused by a deletion, a rename applied in three places out of four.
That is the shape a fix to a Suggestion breaks in, and it is exactly what a
refuter reading one finding is not looking at.

One case takes the batch apart. A refuter re-runs a command its finding cites
having observed, so when any finding going out to one cites an observed test
run, its suite and yours are the same command in the same checkout launched at
once — two runs over one cache, lock or artifact directory, and a result
neither can be trusted. Wait for the refuters before re-running then, and say
the batch was split and why. Nothing else needs the wait: the linter runs in
check mode on both sides, and a refuter holding no test-citing finding starts
no suite.

Run the command the report's **Lint:** line names, not the project's `lint`
script. Where that script fixes in place — `eslint --fix` and friends — the
reviewer already substituted a check-mode invocation, and re-deriving the
command here would throw that away and rewrite the tree mid-verification.

Both re-runs are execution, so they take the trust gate the refuters take. The
check-mode substitution governs what the linter *does*, not whose code it
loads — a suite runs the tree's test files, config and build hooks, and a
linter loads its config and plugins from that same tree. Run neither unless the
tree is the user's own work or they have sanctioned it explicitly. The fix path
already knows which, since it relays that provenance to every refuter it
dispatches, and the session's own permissions are no guard here: a project that
pre-approves its test or lint command runs it unprompted. Otherwise report both
as `not run: tree is not the user's own work`, and say the fixes stand checked
by the refuters alone.

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
  else. The test and lint re-runs above are the only checks that cover
  regressions, which is why their results — `not run: reason` included — belong
  in the report.
- Fixed **Worth doing** Suggestions go unverified — the same economics as the
  verify pass — and are reported as such.
- The refuter defaults to `refuted` when the evidence is ambiguous, and that
  default now lands in the fix's favour — relay a verdict whose reasoning looks
  thin as exactly that. A `stands` whose reasoning says the check was blocked
  (the refuter declined to run the decisive command) is not the fix failing:
  relay it as **could not verify**.
- A fix that reached beyond the finding's own hunk has changed code no refuter
  was pointed at. Offer a fresh `/wtf-code-review` for it instead of
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

The guards that apply to anything published to GitHub live in
`~/.claude/reference/github-publishing.md`, shared with `wtf-code-verify` and
`/wtf-create-pr` rather than restated here. Read it before posting.

### How to post

Default to one inline review comment per finding, anchored at its `file:line`
— it puts each finding where the reader is already looking when they open that
line, and is more GitHub-native than a wall of text. Post them as a single PR
review rather than one API call per comment (a `POST .../pulls/{number}/reviews`
with a `comments` array, or the `gh` equivalent), so they land together as one
review instead of trickling in as separate notifications.

Inline anchoring only works within the PR's diff hunks — GitHub rejects a
comment on a line the diff does not touch. Check each finding's `file:line`
against the hunks *before* posting, rather than discovering the rejection from a
failed call.

On a PR scope those hunks are already in hand: `scope.diff` **is** `gh pr diff`,
because that is the only thing this command will review a PR from. Check against
the artifact rather than re-fetching, and the anchor check and the review are
about one set of bytes instead of two computed at different moments. Anchor each
comment with `path` and `line` + `side` — not the deprecated `position` — and set
the review's `commit_id` to the manifest's `scope_head`, which is the head the
findings were actually read from, so a comment cannot land against a commit
nobody reviewed. A finding's `file:line` always names code that still exists in the
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
