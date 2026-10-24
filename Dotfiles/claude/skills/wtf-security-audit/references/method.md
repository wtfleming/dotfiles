# Method and evidence contract

Find vulnerabilities that cross a real trust boundary, and give the owner the
source trace, the bounded evidence, a priority and the smallest fix. This is
defensive, source-first work. Adapted from Cloudflare's security-audit skill
(MIT, see `LICENSE`). The absence, variant and rejection rules and gate item 7
are condensed and reworded from Visa's vulnerability agentic harness
(Apache-2.0, see `LICENSE-vvah` and `NOTICE-vvah`).

## Work from a concrete invariant

1. Name the lower-trust principal and what it starts with.
2. Name the value, action, state transition or resource selector it controls.
3. Find the control that should reject, bind, isolate, limit or revoke it.
4. Trace the exact source path after that decision, through parsing, identity,
   authorization, normalization, stored or derived copies, and the final sink.
5. Stop at the smallest effect that settles it: a wrong return value, a record
   another principal should not reach, a command run with input it should not
   have.
6. State the source change, at the last trusted decision point, that enforces
   the invariant, and the regression case that proves it.

Read sibling, legacy, batch, retry, cancellation, migration and error paths
that produce the same effect, and compare their controls for equivalence, not
mere presence. Compare what one component guarantees with what the next one
assumes, and at every parser or policy handoff compare canonicalization and
units: case, encoding, normalization, trailing separators, time zones, bytes
against characters. Stop a line of investigation as soon as the invariant is
settled either way.

## Claiming a control is missing

An absence is a claim about every place the control belongs, so establish it by
listing them: search for each site that should carry it — every handler, write
path or entry point of that kind — and say how many there are and how you
searched. Each site that lacks it on its own is its own candidate; sites that
lack it for one shared reason, such as a base class they all inherit, are one
candidate with the strongest trace, as **Fingerprints** has it. A control
applied centrally (middleware, a base class, a framework default) that covers
the site refutes the claim. A record nothing acts on is not the control: a
table of failed attempts that no code reads limits nothing. An absence claim
that does not say what was searched is a guess.

## Variants

When a candidate is confirmed and its root cause could recur, search your scope
for its variants: lexical (the same call or idiom elsewhere), structural (the
same shape under other names) and logical (another path to the same effect that
skips the same control). Phrase the search two or three ways, and record each
search with its hit count, so a clean result can be told from a skipped one. A
variant with the same root cause shares its fingerprint, but establish each
one's conditions and impact separately. Report a variant outside your scope as
not covered, never as a candidate.

Then check the same location for the classes that travel with it: a cookie
missing `HttpOnly` usually lacks `Secure` and `SameSite` too; a route with no
CSRF token often has no authorization check either, which is a separate
candidate; a login with no rate limit often also says which accounts exist.

## The gate

A candidate needs a complete repository-relative trace, from the entry point to
the sink, and the strongest control on that path. Every candidate names:

- the lower-trust principal and what it controls;
- the control that should stop it, and where it fails;
- the principal or resource on the other side, and the concrete result.

These are not findings:

1. A missing best practice with no affected principal or resource.
2. A defence-in-depth gap where another layer already stops the attack. Name
   that layer; it is a hardening note at most.
3. A guess about deployment, proxy, provider, browser or identity behaviour
   the repository does not show. If that fact decides it, the candidate is
   `needs_validation`, not `confirmed`.
4. A principal using authority it already has, or harming only itself.
5. A crash, parser quirk or slow path written up as more than it does.
6. A candidate the source refutes. That is `rejected`, never
   `needs_validation`.
7. A class the code cannot have: a server-side bug in code that only runs in
   the browser, memory corruption in a managed language with no native or
   `unsafe` code, `../` in object-store keys where there are no directories to
   escape, SSRF where the caller sets only the path on a host the server
   already meant to call — trace that as input to that service instead.

## Three verdicts

- **confirmed** — the trace is complete, nothing on it stops the attack, and
  the result is established: by a bounded local check where one can be run, or
  by source that leaves no other reading. Carries a severity and the smallest
  fix.
- **needs_validation** — the trace is real but one specific fact decides it
  and cannot be observed from source or a local check. Names that fact as the
  blocker and the exact check that would settle it. Never carries a severity:
  it is a lead, not a low-confidence finding, and never a parking place for a
  speculative idea.
- **rejected** — a candidate disproved by source, by a visible control, by an
  impossible prerequisite, or by having no meaningful impact. Kept under
  **Ruled out**, so the same unsupported claim is not raised again unless the
  evidence changes.

Because a rejection sticks, it names what refutes it: the guard's `path:line`,
the caller that never passes the value, or the misreading in the claim. A
refutation assumed rather than found — "probably validated upstream" — is none:
read the guard, or the candidate stands on the rest of its trace. A comment, a
suppression marker (`NOSONAR`, `# nosec`, `// safe`) or a PR saying it was
fixed is a claim, not a control, and refutes nothing. When the claimed
mechanism fails but the same code still exposes something to the same
principal, reject the claim and raise what the evidence supports as its own
candidate: log forging that fails can still show a credential written to the
log.

## Severity

Only `confirmed` records have one, and it cannot exceed the demonstrated
impact.

- **critical** — an unauthenticated or outside actor gains code execution,
  full data-store access, credentials, or takeover of the operator's account.
- **high** — an explicit security control is fully defeated with real
  consequences: authentication bypass, another principal's data read or
  written, stored script execution, authenticated code execution, an
  unrequested side effect under the operator's authority.
- **medium** — a real boundary violation with a limited blast radius, uncommon
  preconditions, or consequences confined to a narrow set of resources.
- **low** — disclosure of non-secret internals, or an effect needing sustained
  effort for little gain.

The high/medium test: does the result fully defeat an explicit control for an
action with real consequences, or only weaken it? If you cannot state the
damage, the severity is lower than it feels.

## Fingerprints

Every candidate carries a fingerprint: a short, stable identifier derived from
the root cause in source — the file or module, the function, and the missing
control — such as `lib/app/tickets.ex:save:approval-not-bound`. Lowercase
letters, digits and `._:/-` only, and no line number, severity or verdict. The
same root cause keeps the same fingerprint in every verdict, and derives the
same one on a later run over the same code; when a prior audit report is
supplied, reuse its fingerprint for a root cause it already named. One root
cause reachable from several entry points is one candidate with the strongest
trace; independent missing controls are separate candidates.

## Execution

Reading source is always allowed. Run the tree's code — a test, a small
harness, a fixture — only when the user has said running it is fine, and then
only for the minimum check that settles the claim: dummy principals and data,
no network, nothing persistent. Never probe a deployed system, a real account
or a shared service, and never continue past the minimum effect.
