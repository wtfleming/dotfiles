---
description: Independent code review in a fresh context — recent changes, or a named subject. Diff, tests, lint, structured report. Runs a verified parallel pass per dimension; pass --lite for the single-reviewer version.
argument-hint: "[ref, branch, path or subject — defaults to uncommitted, else the branch, else HEAD] [--lite]"
allowed-tools: Agent, Read, Grep, Glob, Bash(git:*), Bash(~/.claude/scripts/resolve-scope.sh:*), Bash(gh pr view:*)
---

Arguments: $ARGUMENTS

Split those into a scope and the optional flag `--lite`. Everything that is not
the flag is the scope, and the scope may be empty.

The full pass is the default: the reviewer, a dedicated agent per dimension, and
a refuter per finding that survives to verification. `--lite` is the reviewer
alone, with its Criticals and Warnings verified. Where the text below says *under
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
avoid. Hand over the scope and nothing else.

An empty scope is resolved here before dispatch, exactly as a named one is — a bare
invocation is the common case, not a special one, and the lenses launch in the same
batch and cannot wait for the reviewer to settle it. Hand the reviewer the artifact
directory, which is data about where the code lives and no more a cold-start violation
than the scope string is. **Only under `--lite`** may you say the scope is empty and
let the agent work out its own: it runs the same resolver you would, and with no batch
behind it there is nothing to gain by resolving first.

Under `--lite`, run the promotion half of **Triage the Suggestions** the moment
the report returns, before deciding anything else with it. A Suggestion whose
content describes a failure becomes a Warning there, and a Warning is what this
path now verifies — so judging the report by the tiers it arrived with would let
exactly that finding print unchecked, which is the hole the verification exists
to close. It is also what gives a promoted finding somewhere to print: promoted
before the report goes out, it lands in the Warning section; promoted after, it
belongs to a report already printed and to a triage it has been taken out of.

Then print the report verbatim — unless it carries a Critical or a Warning,
promoted ones included, in which case hold it and follow **Verify the top
tiers** first. Do not re-rank the findings, soften them, or defend the code —
you are relaying an independent review, not negotiating with it.

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
count of the ones dropped. That is the only rearrangement allowed, and the
promotion above is the only re-tiering: every other finding goes out as written,
in the tier it arrived in.

**Under `--lite`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, follow **If the user asks for fixes** below.

## Verify the top tiers

Under `--lite` the reviewer's findings reach the user checked by nobody, and
the bias that objection rests on does not depend on how many agents ran: the
agent that wrote a finding is the worst-placed to judge it, alone or in a crowd.

Verifying all of them here would cost what the full pass costs, on the path
chosen for being cheap — and unlike a lens, a refuter cannot ride along in the
reviewer's batch, because a finding has to exist before anything can argue
against it. So this path verifies the two tiers that ask for work:
**Criticals and Warnings**, promoted ones included, and Pre-existing findings at
those tiers.

- Critical is the tier that claims to block the commit, so a false one is the
  most expensive finding in the report: it stops work that should not stop.
- Warning is the tier a reader actually acts on, and the more numerous of the
  two — which is what makes it worth checking rather than a reason to skip it.
  An unchecked Warning is where a `--lite` run spends someone's afternoon on a
  defect that was not there.

This is the one place `--lite` stops being cheap, and the cost is the diff's to
set rather than this command's: a report carrying six Warnings spawns six
refuters. Say the number before you spawn them, as the full pass does, so it can
be refused.

Spawn one `wtf-refuter` per finding, in parallel, dispatched exactly as
**Verify** describes on the full pass — the finding verbatim, plus the scope and
whose work it is, and nothing else.

Then print the report with the refuted findings removed. Say how many were
refuted and why — a dropped finding is reported, not hidden — and if everything
was refuted, say so plainly and treat it as a result worth doubting rather than a
clean bill of health. Both of those go **below the Suggestion triage**, for the
reason the full pass puts its accounting there: the triage is work the reader
might do, and the refutation count is a fact about how the pass ran.

Suggestions go through unchecked, left to that triage, which on this path
verifies nothing and says so.

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
- under `--lite` it takes that same route, because this path now verifies
  Warnings too: promote before the refuters are spawned, since the promotion is
  what puts the finding in front of one at all. Same reporting — under Warning,
  marked **(promoted from Suggestion)**, out of the triage, and a Pre-existing
  one in its own section with the new tier leading it.

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
test file, and it is never promoted — the promotion rule below moves "a new branch
with no test" up to Warning, and an untested invariant reads as exactly that. It is
not: the branch has a test, and this is a second way to exercise it.

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
is what lets a reader see which lens earned its dispatch.

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
Pre-existing format, keeping its **Scope**, **Tests** and **Lint** header lines — the
test result is the most load-bearing line in the report, and on the full pass this
is its only airing. The surviving Suggestions print once, in the triage below.

Do not carry the reviewer's **Correspondence** line into the merged header. The
**Scope** line below already ends in the correspondence, written out as prose, and
the second line restates it as a state word, a SHA and a scratch path — plumbing
this pass has already used. It stays in the reviewer's own template because
`--lite` prints that report verbatim, and there it is the only channel this command
has for the correspondence it must send to its refuters.

The **Scope** line is the manifest's `scope_line`, which already carries the
correspondence. Say it even when it is `same`: a reader cannot tell "the tree
holds the reviewed code" from "nobody checked" unless the report distinguishes
them, and every finding below was read out of one tree or the other. Where
`base_stale` is set, say that too — the scope may be wider than the branch.

A finding promoted from Suggestion to Warning needs nothing said about it here.
It is already marked **(promoted from Suggestion)** where it sits, and the
refutation line below already says how it fared; a second telling in the
accounting is the same disclosure charged twice.

Then the **Suggestion triage**, carrying the Suggestions that remain: the
**Definitely worth doing** list as it came back from the refuters, and the
**Worth doing** list marked **(unverified)**.

**Then, last, the accounting for the pass.** It goes below the triage rather than
between it and the report: everything above it is work the reader might do, and
everything in it is a fact about how the pass ran. Put that first and a reader
crosses agent bookkeeping to reach the advice; put it last and the report can be
stopped at the point the advice runs out.

- how many findings were refuted, and why — a dropped finding is reported, not
  hidden
- **lens coverage, on one line**, each lens labelled with what it returned:

  ```
  Lenses: correctness, security, tests — clean · performance — n/a · dependencies — not dispatched (no manifest changes)
  ```

  Clean, **not applicable** and **not dispatched** are three different facts —
  a lens that governed something and found it clean, one with no surface to
  review, and one **Pick the lenses** excluded — so each lens carries its own
  label and none is folded into another. What they do not need is a list and a
  paragraph each. `not dispatched` keeps the check that excluded it, as a
  parenthetical.
- **which lenses returned no usable report** — errored, timed out, or came back
  unparseable — on a line of its own, never on the line above. An agent that
  failed is not a dimension that came back clean, and counting it as one is how
  a broken pass reads as a passing one. This is the one lens state worth
  interrupting the reader for, because it means the coverage is thinner than the
  report looks.
- **which lenses returned subject not found**, on its own line, should it happen:
  the lenses were handed the file list the reviewer settled, so a lens that still
  could not find the subject disagreed with the reviewer about what implements
  it. That lens reviewed nothing, and printing it as no findings would say the
  opposite.
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then stop. The same close as above: the findings are the user's to triage, and
fixes happen only if they ask — when they do, follow the next section.

## If the user asks for fixes

**Snapshot the tree before the first edit.** The cold review at the end reads the
fix diff, and once the edits have landed nothing separates them from the change
they repair. Take the snapshot first or that review is not available at all, and
take it in two halves, because one command does not cover the tree:

```sh
git stash create                          # tracked changes, as a dangling commit; changes nothing on disk
git ls-files --others --exclude-standard  # the untracked files it does not record
```

Copy each file the second command lists into `<scratch>/pre/`, keeping its
repo-relative path. `git stash create` records **no untracked content**, and an
untracked file is not a corner case here: the resolver folds untracked files into
the reviewed diff, so a finding against a file that has never been committed is
ordinary, and without the copies a fix to one leaves no trace in the diff below.

Empty output from `git stash create` means no tracked change was pending, so `HEAD`
is that half of the snapshot. It says nothing about the untracked half, which is why
the second command runs either way. Where neither half can be had — no commit yet, or
the session will not run the commands — the fix diff cannot be built, and the review
below is reported `not run` with that reason rather than approximated from
`git diff HEAD`: on an uncommitted scope that is the reviewed change and the fixes
together, and a reviewer handed it re-reviews the branch while claiming to have read
the repairs.

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

Spawn one `wtf-refuter`, in parallel, per fixed **Critical** and **Warning** and
per fixed **Pre-existing** finding at either of those tiers. Fixed Suggestions
get none — a promoted one is a Warning by the time it is fixed, so it is covered
by the first clause rather than being an exception to this one.

Pre-existing is named rather than left to "Critical and Warning" because this
command files such a finding in its own section *instead of* the tier's, so an
enumeration of tiers does not reach it — the verify pass spells it out for the
same reason. The user had to name it to get it fixed at all, which makes it the
last finding to check silently.

This once covered every fixed finding, and the session that widened it measured
what that bought: twelve refuters over six fix rounds, every one of them
returning `refuted`, while the cold review below caught three defects those same
refuters had just passed. One session is not a law, but the mechanism it exposes
is structural — a refuter asks whether the old problem is gone, and a fix that
resolves its finding answers yes no matter what else it did. So the fan-out
scales its cost with the finding count and its yield with nothing, and the tiers
kept here are the ones where a fix that quietly did not take costs more than the
agent does.

Send the finding **as the review wrote it**, plus the same scope-and-provenance
data the verify pass sends — here that is the working tree, where the fixes
landed, and whose work it is, so the correspondence you send is `workspace` — and
nothing else: not the fix, not which lines
it touched, not that a fix exists. The finding's `file:line` may have drifted
under the edits; locating the code in the tree as it now stands is the
refuter's job, not a reason to annotate the dispatch. Say how many refuters that
is before spawning them, and that a cold review of the fixes may follow: announce
it as *N* refuters plus one review if the fix diff comes back non-empty. Whether
that agent is dispatched is not settled until the diff is built, and a flat count
of *N* + 1 announced here overstates the spend whenever it is not. A round that
fixed only Suggestions spawns none at all: say the fixes go to the cold review
alone rather than announcing zero refuters.

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
as `not run: tree is not the user's own work`, and name what is left standing
behind the fixes: the fix review, and the refuters where any were spawned. On a
Suggestions-only round there are none, and the fix review is the whole of the
check — say that rather than crediting refuters that never ran. The fix review
establishes that trust for itself and may decline the same runs for the same
reason.

**A `not run` says which kind it was.** This command's `allowed-tools` cannot cover
this section, and not by oversight: the commands come from the reviewer's **Tests:**
and **Lint:** lines, which name whatever the project uses, so there is nothing to
enumerate at the time the frontmatter is written. What it does declare is the review
path plus `Bash(gh pr view:*)` for the stranded-verdict read below, which is a fixed
command and so can be named. Everything else here runs in the main conversation on a
later turn, under the session's own permissions. So `not run` has three distinct
causes — the tree is not the user's work, the session would not permit the command,
or the report named no such command — and they are three different facts about how
far the check reached. Say which; a bare `not run` reads as the first.

The verdicts read inverted from the verify pass: the refuter argues the code is
correct, so against the fixed tree `refuted` means the problem is gone and
`stands` means the fix did not take. Relay them to the user in fix terms —
**resolved** and **fix did not take**, with the raw verdict in parentheses if
fidelity matters — because a user who asked for fixes and reads "3 refuted"
will hear the fixes failing.

Two verdicts do not mean what they say, and each is relayed as what it is rather
than as a standing warning printed every round. The refuter defaults to
`refuted` where the evidence is ambiguous, and on the fixed tree that default
falls in the fix's favour — so relay a `resolved` whose reasoning looks thin as
exactly that. And a `stands` whose reasoning says the check was blocked, because
the refuter declined to run the decisive command, is not the fix failing: relay
it as **could not verify**.

A finding that still stands goes back to the user with the refuter's reasoning
verbatim. Do not quietly take another swing and re-verify — a fix that failed
its check once is a fix a human should look at.

**Then review the fix diff, cold.** A refuter answers one question — is the
finding it holds still there? — and a fix that resolves its finding while
introducing a defect of its own answers that question correctly and says
nothing. This step is for the second half. A round of fixing is itself a source
of bugs, and none of the checks above see them: an assertion a fix made vacuous
passes the suite by construction, and behaviour a fix changed that no finding
mentioned passes the suite and the linter alike.

Build the diff once the refuters and the re-runs have settled, a part per half of
the snapshot:

```sh
git diff <snapshot> > <scratch>/fix.diff                                  # tracked files
git diff --no-index -- <scratch>/pre/<path> <path> >> <scratch>/fix.diff  # untracked when you snapshotted
git diff --no-index -- /dev/null <path> >> <scratch>/fix.diff             # created by the fixes
```

`/dev/null` belongs to the third line alone. Reaching for it on a file that
already existed emits that whole file as added, so the reviewer reads lines the
change under review wrote as lines the fixes wrote — inside a section titled a
review of the fixes, which is worse than leaving the file out.

Read `--no-index`'s exit status per line rather than as one rule. On the third it
is always 1, since a created file always differs from `/dev/null`, and there that
status is the diff rather than a failure. On the second it carries three
different facts, and only one of them is a diff:

- **0, and no output** — the fixes left that file alone. This is the ordinary
  case: the snapshot copies every untracked file and the fixes touch few of them.
- **1, with output** — the file changed, and the hunk is the fix.
- **1, nothing on stdout, `error: Could not access '<path>'` on stderr** — the
  fixes deleted or renamed it, and git will not diff a file that is gone. Nothing
  is appended, so a finding answered by deleting a file the change had just added
  would reach the reviewer as a diff that never mentions it. Record the deletion
  by inverting the operands instead — `git diff --no-index -- <path> /dev/null >>
  <scratch>/fix.diff`, run from `<scratch>/pre`, which emits a `deleted file` hunk
  carrying the repo-relative path. A rename lands here as a deletion and has a
  second half: the path it moved *to* takes whichever line its own presence in the
  snapshot dictates, as any other file does — the second where `<scratch>/pre/`
  holds a copy of it, and only the third where it holds none. Record just the
  deletion and the diff shows the change's new file removed and never re-added,
  which is this bullet's own failure inverted.

Then dispatch a single `wtf-change-reviewer` with the path to that diff and
nothing else: not the findings it answers, not which edit was which, not that
the diff is a set of fixes at all. That is the cold-start rule the original
review runs on, and it binds harder here — you wrote this code minutes ago. It is
the conditional agent from the count above; say that it is being dispatched.

Dispatch it after the refuter batch, never in it. It discovers and runs the
suite and the linter itself, which is the same command in the same checkout as
your re-run above, and that is the collision the split rule already describes.

An empty fix diff means the edits changed nothing on disk — which is what it
means only when every part above came back empty, the untracked ones included.
Say so and dispatch nothing.

What comes back is a review of the fixes, so print it in its own section under
that name, marked **(unverified)** — nothing refutes it, and a verify pass on
top of this one is a regress that ends nowhere. Do not act on it in the same
turn. A fix round that produced its own findings is exactly the sequence a human
should see before another edit lands on top of it; the user asks for a further
round, or does not.

**Only a Critical or a Warning from it is worth another round.** Say that when
you print the report, and name its Suggestions as the half the loop is not gated
on. A second reviewer reading code a first one has just rewritten disagrees with
the rewrite as often as it finds a defect in it — on prose especially, where the
disagreement is taste and each one costs a round that ends in another review.
A Warning is different: every one this step has raised so far was a defect the
fix itself had introduced.

Close with the two gaps that change what the reader does next, and nothing
further — a longer standing disclaimer printed identically every round is one a
reader learns to skip, taking these with it:

- Fixed Suggestions go unverified. The cold review reads them along with
  everything else the fixes touched, but nothing checks the one claim a refuter
  would have checked: that the finding is actually resolved.
- Nothing here re-reads the change as a whole with the repairs in it. The fix
  review reads what the fixes touched and each refuter reads one finding, and
  that gap is widest when the fixes were surgical — offer a fresh
  `/wtf-code-review` over the branch as it now stands.

**A fix can strand a published verdict.** Where the scope is a PR, read its body
(`gh pr view <n> --json body`) for a `<!-- verify:start -->` section and say the
verification is stale, naming what would refresh it (`wtf-code-verify` on the branch as it
now stands). Where the read fails rather than coming back without markers, say **could not
check for a published verification section** — an absent marker and an unreadable body are
different facts, and only one of them is good news. The read is deliberately unbounded,
unlike `resolve-scope.sh`'s own `gh` calls: bounding means a `timeout`/`gtimeout` prefix,
and a grant matches on the command prefix — so `Bash(timeout:*)` would license
`timeout`-prefixed *anything*, far wider than the one read it protects. A narrow grant for
a fixed command is worth more than the bound here, since `gh` setting no client timeout
means a hung read stalls visibly at the end of the run where it can be interrupted, while
a broad grant is permanent. The mechanism, why no delimiter catches
this, and where the note belongs when findings are being posted are in
`~/.claude/reference/github-publishing.md`, which names this command among the tools the
rule binds.

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
does not travel with it. A promoted finding posts as the Warning the review
settled on, marked **(promoted from Suggestion)** — posting it under the tier it
arrived as would be re-ranking just as much as posting it higher, which the next
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
