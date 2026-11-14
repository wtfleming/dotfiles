# Lens: observability

**Looks for:** a new failure path nothing logs, a new endpoint, worker or
scheduled job with no counter or timing for its errors, latency or backlog, an
async handoff that drops the request or correlation context, an error caught and
logged without its cause, a log line carrying no identifier that would let
someone find the affected record, a path the surrounding module instruments that
this one does not, a signal that can drift from what it reports — and telemetry
that costs more than it earns: a line raised in severity or emitted
unconditionally where the code it replaced returned early.

`resilience` asks whether the code survives a failure; this lens asks whether
anyone can tell it happened. Read what the repo already emits before proposing a
remedy, so the remedy uses the telemetry this project has rather than one it
does not.

## Evidence bar

Almost every finding here asserts that something is *not* there. Search before
asserting the absence, and count the emitters that are not in the file: a
middleware that already logs every request, a line the platform emits for free,
a wrapper the call passes through on its way out. "This should probably have a
metric" is not reportable.

## Pre-existing gaps

An absence has usually been there a while. Report one only where *this change*
is what makes it matter: a silent path this diff puts in front of users, a
signal this diff has just made load-bearing. Otherwise you are inventorying the
repo's telemetry rather than reviewing the change.

## Remedies must not leak

Your remedies add data to log output, and nothing downstream checks them. The
identifier you ask for is one that finds the record — a record, request or
correlation id — never an email, an account number or a credential. Logging an
error's cause means its type and message, not an unredacted payload.

## Not applicable

Only when the scope holds no code this lens could govern at all. A silent
failure path is the finding, not a missing surface.
