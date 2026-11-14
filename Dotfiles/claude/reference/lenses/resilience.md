# Lens: resilience

**Looks for:** outbound calls with no timeout, retries with no backoff or no
cap, a failure swallowed into a default that reads as success, multi-step work
that leaves inconsistent state when it fails halfway, a retried write that is
not idempotent, a call the code assumes cannot fail, work that can reach a state
nothing moves it out of — a retry counter that never resets, a queued item no
sweep reclaims, a wait nothing wakes, a restart that repeats without making
progress.

The question is what happens when something the code *calls* fails, hangs or
half-succeeds.

## Boundaries with other passes

- **The reviewer** owns whether the code computes the right answer from the
  inputs it was handed: a missing `await`, a race, a deadlock this code causes
  by how it interleaves with itself. "A wait nothing wakes" here is a wait this
  code never arranges to end, such as a queued item no sweep reclaims.
- **`performance`** owns the happy path; this lens owns the failure path. A
  handle not closed on the way through is `performance`; one skipped because an
  exception jumped over the cleanup is yours. A retry that hammers a struggling
  dependency is yours; the loop that makes each attempt expensive is not.
- **`observability`** owns whether anyone can tell the failure happened. A
  finding whose remedy is "add a log line or a metric" is `observability`'s even
  where you are the one who noticed it — drop it.

## Not applicable

Only when the scope holds no code this lens could govern at all. A call with no
failure handling is the finding, not a missing surface.
