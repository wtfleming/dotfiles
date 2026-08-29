---
description: Early design review of work in progress, in a fresh context — is this the right shape? Suggestion-only; every finding names a concrete, smaller alternative. Run it mid-work, before polishing; /wtf-code-review is the pre-PR gate.
argument-hint: "[ref, branch or path — defaults to uncommitted, else the branch, else HEAD]"
allowed-tools: Agent
---

Arguments: $ARGUMENTS

That is the scope, and it may be empty.

This command runs *early*, on work in progress — the point is to hear "there is
a simpler shape" while changing course is still cheap, not after the code is
polished. It reviews the approach; it does not hunt bugs, and it does not fix.
For the defect review, use `/wtf-code-review` when the work is done.

Launch the `wtf-design-reviewer` subagent on the scope. Dispatch it with the
Agent tool, `subagent_type: "wtf-design-reviewer"`, and wait for it to
complete.

The reviewer starts cold — that is the whole point. The prompt you send it
contains **only** the scope. Do not include:

- your summary of what the change does, or the plan behind it
- the approach you considered and rejected, or why you built it this way
- any reasoning from this conversation

If you wrote the code under review, that is exactly the bias this exists to
avoid: a design reviewer who has read your rationale reviews the rationale, not
the shape. Hand over the scope and nothing else. If the scope is empty, say so
and let the agent work out its own.

Print the report verbatim when it returns. Do not re-rank it, soften it, or
defend the code — you are relaying an independent review, not negotiating with
it.

Then stop. The suggestions are the author's to weigh — a design review is
advice, not a mandate, and the author knows the constraints and sunk cost the
reviewer cannot see. If the user replies asking to take a suggestion, do the
rework they name in the main conversation, and leave committing to them.
