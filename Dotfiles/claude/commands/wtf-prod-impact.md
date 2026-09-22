---
description: Assess whether a change would degrade production once deployed, by reading the live system — deployed revision, real traffic on the changed path, what is already failing, and whether the change is gated. Provider-agnostic and read-only; returns go / no-go / not assessed.
argument-hint: "[ref, branch or path — defaults to uncommitted, else the branch, else HEAD]"
allowed-tools: Agent
---

Arguments: $ARGUMENTS

That is the scope, and it may be empty.

This command asks one question — would this change degrade production once merged
and deployed — and answers it from the live system rather than from the diff. It is
the pre-deploy gate, and it is complementary to the other two: `/wtf-code-review`
reads the code, `wtf-code-verify` runs it in a disposable environment, and neither
of them knows anything about the system the change is about to land on.

It is read-only against production. It does not fix, and it does not review code.

Launch the `wtf-prod-impact` subagent on the scope. Dispatch it with the Agent
tool, `subagent_type: "wtf-prod-impact"`, and wait for it to complete.

The prompt you send it contains **only** the scope. Do not include:

- your summary of what the change does, or why it is safe
- the testing or rollout plan behind it
- any reasoning from this conversation

If you wrote the change, your belief about its blast radius is the thing this pass
exists to check independently. Hand over the scope and nothing else. If the scope
is empty, say so and let the agent work out its own.

Print the report verbatim when it returns. Do not re-rank the trajectories, soften
the verdict, or argue with a **no-go** — you are relaying an assessment, not
negotiating with it.

**Relay a "not assessed" verdict as exactly that.** It means the agent could not
read production, and it is the one outcome most likely to get quietly rewritten as
reassurance on the way to the user. It is not a pass, it is not "no issues found",
and it does not become either by being restated. If you cannot tell which verdict
came back, say so rather than picking the comfortable one.

The report carries four sections below the verdict — **Trajectories**,
**Unanswered**, **Calls made**, and the **As of** / **Mapping** / **Providers**
header lines. If any is missing, say which rather than filling it in: the mapping
and the unanswered questions are how a reader checks the assessment, and an
assessment nobody can check should not read as one. They are named here because
this command has only the `Agent` tool and cannot read the agent's own copy of the
list.

Then stop. A **no-go** names what would make the change safe; doing that is the
author's call, in the main conversation where the constraints live, not here.
