---
description: Independent code review of recent changes in a fresh context — diff, tests, lint, structured report. Pass --fix to triage the findings interactively afterwards.
argument-hint: "[ref, branch or path — defaults to uncommitted changes] [--fix]"
allowed-tools: Agent, Task, AskUserQuestion, Read, Edit, Grep, Glob, Bash(git:*)
---

Arguments: $ARGUMENTS

Split those into a scope and an optional `--fix` flag, in either order.
Everything that is not `--fix` is the scope, and the scope may be empty.

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

**Without `--fix`, stop here.** The findings are the user's to triage.

## With `--fix`

### Verify first

The reviewer judged its own findings, which is the position it is worst placed
to judge from. Before offering to act on any of them, check them independently.

Spawn one `wtf-refuter` subagent per finding, in parallel. It already knows to
argue the code is correct, to re-run a command the finding claims to have
observed, and to default to `refuted` when it cannot decide.

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

### Then walk through what survives

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
