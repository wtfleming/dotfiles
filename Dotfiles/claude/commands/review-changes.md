---
description: Independent code review of recent changes in a fresh context — diff, tests, lint, structured report
argument-hint: "[ref, branch or path — defaults to uncommitted changes, else the branch]"
---

Launch the `change-reviewer` subagent to review: $ARGUMENTS

Dispatch it with the Agent tool, `subagent_type: "change-reviewer"`, and wait for
it (`run_in_background: false`) — the review is the deliverable of this turn.

The whole point is that the reviewer starts cold. So the prompt you send it
contains **only** the scope to review. Do not include:

- your summary of what the change does or why
- which parts you think are fine, or which you are unsure about
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid. Hand over the scope and nothing else. If `$ARGUMENTS` is empty, say so
and let the agent work out the scope itself.

When it returns, print its report verbatim. Do not re-rank the findings, soften
them, or defend the code — you are relaying an independent review, not
negotiating with it.

Then stop. Do not fix anything unless asked; the findings are for the user to
triage.
