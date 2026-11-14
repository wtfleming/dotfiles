---
description: Review a design doc, RFC, ADR or technical proposal in a fresh context — checks its claims about the current system against the code and production telemetry, then judges whether it is ready to approve. Read-only. For "is there a smaller shape" on a plan, /wtf-design-review also takes one.
argument-hint: "<path, link, or pasted text of the document>"
allowed-tools: Agent
---

Arguments: $ARGUMENTS

That is the document under review. If it is empty, ask which document and stop.

Launch the `wtf-doc-reviewer` subagent on it. Dispatch it with the Agent tool,
`subagent_type: "wtf-doc-reviewer"`, and wait for it to complete.

The reviewer starts cold. The prompt you send it contains the document — its
path, its link, or its text exactly as the user gave it — and nothing else. Do
not include:

- your summary of the document, or of the problem it solves
- what you or the author think of it, or why it was written this way
- any reasoning from this conversation

If you helped write the document, that is exactly the bias this exists to avoid:
a reviewer who has read the rationale reviews the rationale.

Print the report verbatim when it returns. Do not re-rank it, soften it, or
defend the document. The report always ends with **Claims checked** and
**Coverage**; if either is absent, say so rather than filling it in.

Then stop. If the user asks to act on the review, revise the document in the
main conversation.
