# Attack classes

Choose the classes that apply to what the code actually does. Each is a place
to look, not a checklist to report against: every candidate still passes the
gate in `method.md`. Adapted from Cloudflare's security-audit skill (MIT).

**Injection.** Trace untrusted input to a dangerous sink: SQL or query
construction, HTML or template output, shell commands and their arguments,
file paths, redirects, deserialization, regexes, log lines. Look for indirect
injection — data stored safely, then used in a dangerous context by different
code — and for injection through keys, field names, headers and metadata, not
only values.

**Access control.** Verify a caller cannot act outside its authority, not just
that a check exists: the right permission, on the right resource, through the
right mechanism. Is there another path to the same state change with a weaker
check? Can a request field override what the check meant to restrict? Does a
route authenticate but forget to authorize? Do bulk, batch, export and import
paths enforce per-item permissions?

**Sessions and requests.** Session fixation and invalidation on login and
logout; cookie scope and flags where the cookie carries authority; CSRF on
state-changing requests, including websocket and LiveView events; open
redirects through `redirect`, `return`, `next` or `url` parameters; trust in
`Host` or forwarded headers. A socket that authenticates by cookie and does
not check `Origin` (LiveView's `check_origin`) can be driven by any page the
operator visits, including for an app served from localhost.

**OAuth and keys.** An OAuth or OIDC flow whose request and callback are not
bound to each other: a missing or unchecked `state`, a callback that accepts a
code another session started, a device login whose approval is not tied to the
session that polls for it. An API key sent to a host other than the one it was
issued for.

**Resources and files.** Path traversal through `..`, symlinks, encoded
sequences or absolute paths; SSRF through user- or model-supplied URLs,
redirects and parser differentials; archive extraction outside the target; temp
files; a check on a path and a use of it that can race.

**Secrets and cryptography.** Hardcoded credentials; secrets in logs, error
messages, URLs, rendered pages or anything a model is shown; weak randomness
for tokens; non-constant-time comparison of secrets; an error path that falls
back to no crypto.

**Business logic.** Skipped, repeated or reordered steps in a state machine;
partial failure that leaves step one applied; check-then-act races with
business impact; negative, zero or overflowing quantities; trust in data
"validated on the way in" that another path also writes; expiry and clock
edges; the security posture when config is missing, a flag is off, or a
dependency is down.

**Feature abuse.** Export or backup that includes more than the caller may
read; import that bypasses validation or overwrites; search, filter or error
differences that reveal what the caller cannot see; webhook, callback or
notification URLs the server fetches.

**Chained trust.** Component A validates and B assumes a stronger guarantee
than A produced — truncation, coercion, normalization, scope. A value safe when
stored becomes dangerous later: a field name becomes a path, escaped text is
rendered raw, a string becomes a URL, regex, template or command. Token, plugin
or agent capabilities that grow through delegation, refresh, caching or
composition. Rollback, restore and cancellation that skip current
authorization.

**Supply chain and automation.** Untrusted input in a privileged CI workflow
or build step; mutable or unpinned build inputs; an automation identity with
more reach than its job; vendored code nothing tracks. Trust mixing between
caches, artifacts and workspaces: a worktree or build seeded with another
tree's dependencies or compiled output runs that tree's code. Plugins and
extensions — skills, MCP servers, hooks — that gain trust by being loaded
rather than approved. An agent or job that runs a repository's own scripts,
tests or build with credentials in reach.

**Local helpers.** A privileged helper — a sandbox daemon, a remote workspace
host, a credential helper — acting as a confused deputy for a less-trusted
caller. A local file whose ownership or permissions let another process
replace it before it is trusted. A credential store or local secret readable
from a boundary meant to be cut off from it, such as a sandboxed process
reaching the host's tokens.

**Resource exhaustion and spend.** Work that outlives its cancellation: a
detached task, stream or worker still running and spending after the request,
run or session that started it is gone. Retries with no cap or backoff, and a
fallback that turns one failure into many calls. A poison record that fails on
every pass and blocks what queues behind it. Unbounded buffering, fan-out or
key cardinality. In an agent harness the resource is often money: model calls,
agent runs, scheduled jobs. The gate still applies — the operator spending
their own budget is self-impact, so a finding needs a lower-trust principal
(repository content, ticket text, model output, an MCP peer) driving the work.

**The strange corner.** Read code that looks unrelated to security:
half-finished, experimental, compatibility and fallback paths; API calls the UI
never makes; features combined in ways nobody designed; a comment explaining
why something is safe — verify the explanation; anything named `temp`, `hack`
or `legacy`; edge cases the tests do not cover.

**Obvious things.** A literal sweep of your scope, not a creative one:
hardcoded secrets and committed key files; security TODO, FIXME and HACK
comments; debug or dev mode reachable in production; seed or default
credentials; unprotected debug, admin or health endpoints; `.gitignore` gaps;
unpinned dependencies and known CVEs in lockfiles; dynamic `eval` or code
loaded from strings; permissive CORS; cookie flags; open redirects; stack
traces in error responses. A flag is not a finding until its impact is traced
through the gate.
