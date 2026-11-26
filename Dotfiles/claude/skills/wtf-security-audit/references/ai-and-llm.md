# AI, LLM and agent classes

For code where a model takes part in a trust-sensitive decision: prompt
assembly, agent and tool loops, approval gates, MCP clients and servers,
persistent memory, and code that acts on model output. The flow that matters is
untrusted content, into a model or memory, out to a capability, an authority or
a sink. Use these beside `attack-classes.md`, not instead of it.

## Discipline

- Prompt injection alone is not a finding. The finding is a code-level failure
  after it: content reaches another principal's context, invokes authority the
  attacker lacks, discloses data they cannot read, or drives a sink they cannot
  reach directly.
- Model output, memory, tool descriptions and MCP responses are untrusted
  input. Point to the code that grants authority, trusts output, writes durable
  state or feeds a sink.
- A guardrail prompt is not a security boundary. Only deterministic checks
  count: resource-scoped authorization, isolation, binding, constrained
  credentials, a sandbox that reaches nothing.
- Authorization and action binding are separate controls. Attacker-written
  content that causes an action under the operator's valid authority is an
  action-binding failure when the operator did not request or approve that
  exact action.
- An intentional request to use the requester's own authority is not a
  delegation defect merely because a model carried it out.

## Context and memory

**Indirect injection.** Who can write each source that enters a model's
context — a file in the tree, a ticket or comment, a web page, command output,
a tool or MCP result — and what capability does the consuming dispatch hold?
The defect is a missing deterministic control, not persuasive text.

**Provenance confusion.** Untrusted text placed where it reads as a system
message, an earlier turn, a tool result or policy: string concatenation,
caller-controlled role fields, fences the quoted text can close, summaries that
lose source labels. Confirm the forged provenance changes a deterministic
decision or reaches a capability.

**Persistent poisoning.** Attacker-influenced content or a model summary
written somewhere a later, more privileged task reads as instructions or
facts. Cite both the write and the later privileged read.

**Invisible text.** Content a model reads and a person does not: Unicode tag
characters (U+E0000–U+E007F, which mirror ASCII), zero-width characters, bidi
overrides, an HTML comment, text set into an image. Where the only control on
a source is that someone reviews it, that review did not see this.

## Tools and actions

**Tool-argument injection.** Model-produced arguments reaching a shell, file,
query, URL fetch or privileged API without handler-side validation. A schema
narrows shape; it does not establish safe paths, safe URLs or authorization.

**Excessive agency.** A dispatch holding a broad credential or an unconfined
shell where its job needs less; a tool handler that does not re-check what the
requester may do.

**Approval binding.** The operator approves one described action, and
execution can use changed arguments, a different target or a later turn — or
the tool is reachable without the approval being recorded at all. Check that
approval binds the normalized tool, its complete arguments and the target, and
that a retry or resumed session cannot reuse it.

**Schema and dispatcher disagreement.** Aliases, extra fields, coercions or
defaults that the validator and the handler read differently.

## Sub-agents and MCP

**Trust inheritance.** A delegated task receiving the whole session,
credentials or tools rather than the least it needs, or a delegated result
trusted on return.

**Identity confusion.** Calls or results routed by names, aliases or IDs an
attacker can influence rather than by the authenticated connection; two
servers claiming one tool.

**Metadata as policy.** Tool descriptions, annotations or schemas from a peer
treated as granting capability.

## Output

**Insecure rendering.** Model output reaching an HTML, Markdown, Mermaid,
template, URL or command sink without the encoding and policy that sink needs.

**Context extraction.** A credential, another principal's data or private
source in an assembled context whose output a less trusted party can read.

---

Adapted from Cloudflare's security-audit skill (MIT, see `../LICENSE`). The
invisible-text class is adapted from Claude-BugHunter (CC BY 4.0, see
`~/.claude/reference/LICENSE-bughunter`).
