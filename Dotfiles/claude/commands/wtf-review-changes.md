---
description: Independent code review of recent changes in a fresh context — diff, tests, lint, structured report. Pass --fix to triage findings interactively, --deep to add a parallel per-dimension pass.
argument-hint: "[ref, branch or path — defaults to uncommitted, else the branch, else HEAD] [--deep] [--fix]"
allowed-tools: Agent, Task, AskUserQuestion, Read, Edit, Grep, Glob, Bash(git:*)
---

Arguments: $ARGUMENTS

Split those into a scope and the optional flags `--deep` and `--fix`, in any
order. Everything that is not a flag is the scope, and the scope may be empty.

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

**With no flags, stop here.** The findings are the user's to triage.

## With `--deep`

One reviewer covering five dimensions gives some of them a shallower pass than
the others. This adds a dedicated pass per dimension, over the same diff the
reviewer just read.

Say up front how many agents you are about to spawn, so the cost is the user's
to refuse before it is spent rather than after.

Dispatch these five `wtf-lens` subagents **in parallel**, each with the scope and
its own rubric and nothing else:

| Lens | Looks for |
|---|---|
| `correctness` | logic errors, off-by-one, wrong operator, null/empty/zero/max edges, races, unhandled promises, missing await |
| `security` | unvalidated input at boundaries, hardcoded secrets, injection, sensitive data in logs and errors, authz gaps |
| `tests` | new branches with no test, uncovered edge cases, tests that cannot fail, flakiness, fixtures that hide the bug |
| `maintainability` | unclear names, duplicated logic, functions doing several things, unactionable error messages, comments explaining *what* |
| `performance` | N+1 queries, work inside loops that belongs outside, resource leaks, blocking calls in async paths, unbounded growth |

There is deliberately no linter lens. The reviewer already ran the project's real
linter and reported it; a model imitating static analysis is strictly worse than
the tool that does it exactly.

### Synthesise

Merge the five reports with the reviewer's own. Deduplicate on file and line —
where two agents found the same thing, keep the more specific statement and drop
the other, rather than listing it twice with different wording. Where they
disagree on tier, take the higher and say both lenses saw it.

Print the merged report in the reviewer's own Critical / Warning / Suggestion
format. Note which lenses returned nothing; a silent lens is information, and
hiding it makes five agents look like one.

## With `--deep` or `--fix`

### Verify

Every finding so far was judged by the agent that wrote it, which is the position
it is worst placed to judge from. Check them independently before going further.

Both flags need this, for different reasons. Under `--fix` you are about to edit
the tree, and a wrong finding becomes a wrong change. Under `--deep` you have
five agents each under quiet pressure to justify their dispatch, which is exactly
the pressure that produces plausible findings that are not real.

Spawn one `wtf-refuter` subagent per finding, in parallel. It already knows to
argue the code is correct, to re-run a command the finding claims to have
observed, and to default to `refuted` when it cannot decide.

How many that is depends on what the earlier passes found, so it cannot be
announced with the lens count. **Say the number once you know it**, before you
spawn them, and say what it brings the run's total to. The count that scales
with the diff is the one worth disclosing, and it is the one the user has not
already agreed to.

Under `--fix`, refute every finding you will offer to act on — a wrong finding
becomes a wrong edit, and that is the whole reason this gate exists.

Under `--deep` without `--fix`, refute the Critical and Warning findings only.
Nothing is going to be edited, Suggestions are the most numerous tier, and one
agent apiece to verify a naming nit is the bulk of the spend for the least of
the value. Carry the Suggestions through to the report marked **(unverified)**
and say how many went unchecked. Never drop them silently to save the spawn —
an unverified finding the reader knows is unverified is honest; one that
disappears is not.

**Send it the finding verbatim and nothing else.** Not why you think it might be
wrong, not where you would look first, not that you already checked something.
This is the same rule as the reviewer dispatch above and it exists for the same
reason: if you wrote the code, a hint about where the refutation lies is you
arguing your own case through an agent spawned to judge it. A refuter you
steered has told you nothing.

Report how many findings were dropped and why. Do not walk the user through a
finding that was refuted.

If everything is refuted, say so plainly and treat it as a result worth doubting
rather than a clean bill of health — that is also what a gate that never bites
looks like.

Under `--deep` without `--fix`, print what survived and stop there.

## With `--fix`

### Walk through what survives

Take them in order: Critical, then Warning, then Suggestion. For each one, show
the finding and ask with AskUserQuestion:

- **Apply the fix** — make the edit, then move on
- **Show me the change first** — print old vs new, then ask again
- **Skip** — leave it, move on
- **Stop here** — list what is left unaddressed and finish

AskUserQuestion always offers "Other", so the user can type a different fix or
ask a question about the finding without a dedicated option for either. Handle
whatever they type, then re-ask.

Edit only the file the finding names, and only what the finding describes. If
fixing it properly means changing something else, say so and ask first — a
review is not a mandate to refactor.

### Finish

Summarise: how many findings, how many refuted, how many fixed, how many
skipped. Then stop.

**Do not commit.** The user commits their own work, per topic, and a review that
commits on your behalf takes that decision away.
