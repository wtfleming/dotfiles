---
name: wtf-security-audit
description: >-
  Audit one area of a codebase for vulnerabilities that cross a real trust
  boundary — a path, a module, or prose naming a subsystem ("the approval
  gate", "how tool output reaches the shell"). Traces each lead from the
  lower-trust principal to the sink, tries to refute it, and reports confirmed
  findings, leads that need one outside fact, and the claims it ruled out. Use
  when asked to security-audit, threat-hunt or pentest-read an area of the
  code: "security audit the auth module", "is the webhook handler safe",
  "/wtf-security-audit". Read-only: it describes fixes, it does not make them.
  NOT a diff review — the security lens of /wtf-code-review covers a change.
argument-hint: "[path, module, or prose naming the area to audit]"
---

Arguments: $ARGUMENTS

That is the area to audit. It may be empty.

# Audit an area for boundary failures

You hunt vulnerabilities that cross a real trust boundary in one area of this
tree, try to kill each one yourself, and report what survives. You change
nothing: the report describes the smallest fix, and a person decides.

Read `references/method.md` before you start. It holds the invariant method,
the gate every candidate must pass, the three verdicts and the severity
anchors, and the report below depends on all of them. Then read `references/attack-classes.md`, and `references/ai-and-llm.md` whenever a
model, an agent, a tool call, an approval gate or model output is anywhere in
the area.

## Scope

- A path or module: audit that, following calls out of it as far as the
  boundary takes you.
- Prose naming an area: search for the files that implement it, name them in
  the report, and audit those.
- Empty: audit the entry surfaces — where requests, events, files, command
  output, tool results or model output first enter — rather than everything.
  Say which ones you chose.

The scope is a coverage obligation, not a fence: follow a trace wherever it
goes, but report only what starts or ends inside it, and list any separate
boundary you noticed outside it under **Not covered**.

## Map before you hunt

Write down, briefly, before reading for bugs:

1. The principals: who or what can put input into this area — the operator,
   a repository author, a ticket or comment author, a web page, a model, a
   tool or MCP peer — and what each is allowed to do by design.
2. The entry surfaces, the sinks, and the controls between them.

Then hunt class by class from the references. For each lead, apply the gate.
For each candidate that passes, try to refute it before you keep it, as the
method's verdict rules describe.

## Running code

You may read anything. Run the tree's code — a test, a small harness, a
fixture — only when the user has said running it is fine, and then only the
minimum check that settles the claim: dummy principals and data, no network,
nothing persistent. Never probe a deployed system, a real account or
credential, or a shared service, and never continue past the minimum effect.
When
a check you could not run would decide a candidate, the verdict is
`needs_validation` with that check as the plan.

## Report

End with this, and nothing after it:

```markdown
## Security audit: <area>

**Read:** <the files and the entry surfaces you covered>
**Principals:** <one line each>
**Searched:** <each absence and variant search, with its hit count>

### Confirmed
- **<severity>** · `path:line` — <principal> can <action> because <missing
  control>, so <result>. Evidence: <the trace, or the command and its
  output>. Fix: <smallest source change, and the regression case>.

### Needs validation
- `path:line` — <claimed root cause>. Blocked on: <the one fact>. Settle it
  by: <the exact check>.

### Ruled out
- `path:line` — <claim>: <what refutes it, and where>.

### Hardening notes
- <defence-in-depth gaps that are not findings, one line each>

### Not covered
- <boundaries noticed outside the scope, or classes you did not reach>
```

An empty **Confirmed** section is a real and useful answer; never manufacture
a finding to fill it. Leave out any section with nothing in it except
**Confirmed**, which always appears.
