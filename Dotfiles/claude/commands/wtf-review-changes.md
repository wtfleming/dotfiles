---
description: Independent code review of recent changes in a fresh context — diff, tests, lint, structured report. Pass --deep to add a verified parallel per-dimension pass.
argument-hint: "[ref, branch or path — defaults to uncommitted, else the branch, else HEAD] [--deep]"
allowed-tools: Agent, Task, Read, Grep, Glob, Bash(git:*)
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
(`run_in_background: false`).

The whole point is that the reviewer starts cold. So the prompt you send it
contains **only** the scope. Do not include:

- your summary of what the change does or why
- which parts you think are fine, or which you are unsure about
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid. Hand over the scope and nothing else. If the scope is empty, say so and
let the agent work out its own.

Print the report verbatim when it returns. Do not re-rank the findings, soften
them, or defend the code — you are relaying an independent review, not
negotiating with it.

**Without `--deep`, stop here.** The findings are the user's to triage, and the
close matters: do not launch into fixing anything. If the user replies asking
for fixes, make only the edits the named findings describe — a review is not a
mandate to refactor — and leave committing to them.

## With `--deep`

One reviewer covering six dimensions gives some of them a shallower pass than
the others. This adds a dedicated pass per dimension, over the same diff the
reviewer just read — plus `reuse`, which the reviewer's checklist does not cover
at all.

Say up front how many agents you are about to spawn, so the cost is the user's
to refuse before it is spent rather than after.

Dispatch these seven `wtf-lens` subagents **in parallel**, each with the scope
and its own rubric and nothing else:

| Lens | Looks for |
|---|---|
| `correctness` | logic errors, off-by-one, wrong operator, null/empty/zero/max edges, races, unhandled promises, missing await |
| `security` | unvalidated input at boundaries, hardcoded secrets, injection, sensitive data in logs and errors, authz gaps |
| `tests` | new branches with no test, uncovered edge cases, tests that cannot fail, flakiness, fixtures that hide the bug |
| `maintainability` | unclear names, functions doing several things, unactionable error messages, comments explaining *what*, changes bundling unrelated concerns |
| `reuse` | logic the repo already implements elsewhere, a second copy of something within the diff itself, a hand-rolled version of what a dependency already in the manifest provides, a new abstraction where an existing one would have served — and the reverse: code shared between two things that only look alike |
| `performance` | N+1 queries, work inside loops that belongs outside, resource leaks, blocking calls in async paths, unbounded growth |
| `dependencies` | new dependencies (necessity, maintenance, transitive weight), breaking changes to public interfaces, config formats or CLI flags, irreversible migrations, new failure paths nothing logs |

There is deliberately no linter lens. The reviewer already ran the project's real
linter and reported it; a model imitating static analysis is strictly worse than
the tool that does it exactly.

The `tests` lens judges coverage by ROI: a new branch with no test is a finding;
trivial code without one is not.

The `reuse` lens is the one lens that must search outside the diff to do its job
at all — the duplicate it is looking for is, by definition, in code the change
did not touch. Tell it that a finding names the existing implementation by
`file:line`; "something like this probably already exists" is the shape this lens
fails in, and it is not reportable. Duplication is also the finding most often
worth leaving alone, so it judges by whether the two copies have to change
together, not by how alike they look.

### Synthesise

Merge the seven reports with the reviewer's own. Deduplicate on file and line —
where two agents found the same thing, keep the more specific statement and drop
the other, rather than listing it twice with different wording. Where they
disagree on tier, take the higher and say both lenses saw it.

Do not print the merged report yet — it has not been verified, and findings that
are about to be retracted should not get a first airing.

### Verify

Every finding so far was judged by the agent that wrote it, which is the
position it is worst placed to judge from — and under `--deep` there are seven
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

**Send it the finding verbatim and nothing else.** Not why you think it might be
wrong, not where you would look first, not that you already checked something.
This is the same rule as the reviewer dispatch above and it exists for the same
reason: if you wrote the code, a hint about where the refutation lies is you
arguing your own case through an agent spawned to judge it. A refuter you
steered has told you nothing.

### Report

Print the merged report of what survived, in the reviewer's Critical / Warning /
Suggestion format. Then:

- how many findings were refuted, and why — a dropped finding is reported, not
  hidden
- which lenses returned nothing; a silent lens is information, and hiding it
  makes seven agents look like one
- if everything was refuted, say so plainly and treat it as a result worth
  doubting rather than a clean bill of health — that is also what a gate that
  never bites looks like

Then stop. The same close as above: the findings are the user's to triage, fixes
happen only if they ask, and committing is theirs.
