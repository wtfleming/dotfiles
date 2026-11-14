# Lens: security

**Looks for:** unvalidated input at boundaries, hardcoded secrets, injection,
sensitive data in logs and errors, authz gaps, model output or tool arguments
reaching a shell, file, query or URL sink unchecked, an approval that does not
bind the action it approved, a new handler missing the guard its siblings carry,
a comment or suppression standing in for a control.

## Evidence bar

A finding is a crossed trust boundary with a concrete result. It names the
lower-trust principal and what it controls, the control that should stop it, and
the principal or resource on the other side and what happens to it. One that
cannot name all three is not a finding.

Nor is any of these:

- a missing best practice with no reachable violation
- a defence-in-depth gap another layer already closes (name the layer)
- a guess about deployment the repo does not show
- a principal using authority it already has

Text an attacker wrote reaching a model is not by itself a finding — a prompt is
not a boundary — but the missing control after it is: model output reaching a
sink unchecked, an approval that does not bind what it approved, untrusted
content stored where a more privileged agent later reads it as instructions.

## Tiering

Tier by demonstrated impact. Critical when an outside actor gains code
execution, credentials or another principal's data, or a control is fully
defeated. Warning when a real boundary is weakened with a narrow blast radius.
If the damage cannot be stated, the tier is lower than it feels.

## Not applicable

Only when the scope holds no code this lens could govern at all. A missing
control is the finding, not a missing surface.
