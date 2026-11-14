# Lens: tests

**Looks for:** new branches with no test, uncovered edge cases, tests that
cannot fail, flakiness, fixtures that hide the bug, a check the change switches
off — a skipped or disabled test, a lint rule disabled or suppressed, `|| true`
or `continue-on-error` on a check, an edit loosening a check's own config — and
tests that cost more than they catch: assertions that restate the implementation
or reach past the public API, assertions on a stub or on the test's own setup,
tests of trivial code or static values, exact or whole-value assertions on
output the test is not about, the same behaviour asserted again at another
level.

Judge coverage by return: a new branch with no test is a finding; trivial code
without one is not. An uncovered edge case is a finding only where the code can
reach it.

## Tests the change adds or edits

`wtf-change-reviewer` applies this section too.

For each such test, name the production line whose deletion, inversion or
changed value turns it red. A test with no such line cannot fail, and is a
missing test for the behaviour it claims to cover: report it as one, tiered as
one, saying which line you looked for.

Four things look like a test that only restates the code and are not: a
regression test for a real bug; a check on a call where the call is the
behaviour, a side effect outside the process; a golden file or snapshot where
that output is the contract; a characterization test pinning code before a
refactor.

## Tests that cost more than they catch

A different finding from a test that cannot fail: a Suggestion to delete or
narrow it, and it stays a Suggestion, since a test that exists is not a missing
one. The sign of a test that restates the implementation is a refactor that had
to edit its assertions. The four exceptions above apply here as well.

## A check the change switches off

A Warning, counted only on lines the change adds: a suite that goes green by
skipping its failing test has not been fixed. Where the change gives a reason,
quote it in the finding rather than dropping the finding for it.

## Not applicable

Only when the scope holds no code this lens could govern at all. Code with no
tests is the finding, not a missing surface.
