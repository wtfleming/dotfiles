# Lens: performance

**Looks for:** N+1 queries, work inside loops that belongs outside, resource
leaks, blocking calls in async paths, unbounded growth.

## Boundary with `resilience`

This lens owns the happy path — what the code costs when it works and the input
is large. `resilience` owns the failure path. A handle not closed on the way
through is yours; one skipped because an exception jumped over the cleanup is
`resilience`'s. The loop that makes each attempt expensive is yours; a retry
that hammers a struggling dependency is not.
