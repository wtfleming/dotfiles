# Promoting a probe to a permanent test

A probe proves something once. A test proves it on every push. But a suite is a shared
cost, and a slow flaky test is worse than no test — it gets retried, then skipped, then
deleted, and it takes the credibility of its neighbours with it. So promotion is a
judgement, not a reflex.

## Contents

- Triage
- Where the test goes
- Match the idiom
- Prove it fails without the fix
- When the project has no harness at that level
- Clean up what you did not promote

## Triage

| Promote when | Leave throwaway when |
| ------------ | -------------------- |
| it is deterministic across runs and machines | it depends on timing, ordering, or a live external service |
| it fits the project's existing harness | it would need a harness the project does not have |
| it is fast enough that nobody is tempted to skip it | it boots the whole stack to observe one field |
| its input is realistic and reproducible | its input is data you hand-made for the occasion |
| it asserts on a contract | it asserts on wording, layout, or a message that will legitimately change |
| the thing it catches could plausibly come back | it pinned a one-off migration or a deleted code path |

The best promotion candidate is usually a tier-0 or tier-1 probe covering a **negative**
case, because those are the ones nobody writes by hand and the ones a future refactor
silently breaks. A positive-path test often already exists in some form.

Present the triage as a numbered list with a recommendation and a reason per probe, then
ask. Do not write anything until the user says yes — and if they promote something you
advised against, promote it as asked and say once what it will cost.

## Where the test goes

Find the closest existing neighbour rather than working from this table; it is here for
orientation when the tree is unfamiliar.

| | Tests live in |
| --- | --- |
| **Node** | `*.test.ts` beside the source, or `test/` / `__tests__/`; browser-level in `e2e/` |
| **Rust** | unit tests in-module under `#[cfg(test)]`, integration tests in `tests/` |
| **Elixir** | `test/`, mirroring `lib/`; slow ones behind `@tag` |
| **Erlang** | `test/` as eunit or common_test suites |
| **Elisp** | `*-test.el` beside the source |

`environments.md` carries the invocation for each of these — how to scope a run to one
file, name or crate — and is the one place to change when a runner's flags move.

If a test needs a database, use the project's existing isolation rather than inventing
one — Ecto's SQL sandbox, a transactional fixture, a per-test schema. A promoted test
that leaves rows behind will break its neighbours, and the failure will look like their
bug rather than yours.

## Match the idiom

Rewrite the probe into the project's own shape; do not drop a `curl` script into `tests/`
and call it promoted. Read the nearest existing test first and copy its structure —
naming, setup, fixtures, assertion style, how it builds a user, how it names a file.

Two specifics worth getting right:

**Name the test after the behaviour, not the code.**
`archived_posts_are_hidden_from_anonymous_callers` survives a rename of the function it
exercises; `test_posts_query_2` does not, and tells the next reader nothing when it goes
red at 6pm.

**One comment earns its place here**: what the test is defending against, and where to
read more — the issue, the PR, one line. A regression test's *why* is invisible from its
assertions, and a future reader deciding whether it is safe to delete has nothing else to
go on. Keep it to the line a reader needs at that spot; the reasoning belongs in the PR.

## Prove it fails without the fix

The promoted test must fail against the code that lacked the fix, and pass with it. A
test that passes on both sides asserts nothing, and unlike an unverified probe it will be
trusted for years and cited as coverage.

The baseline worktree is probably still up, so this costs one run. The test file does not
exist there, so copy it in rather than committing it into the baseline tree — see
`differential.md`. If the baseline reports *no such test* or *no such file*, the test
never ran and the check proved nothing.

For a promoted test that guards a **new capability** rather than a fix, the base has no
surface to test and this check cannot run. Substitute the deliberate break: revert the
one line the capability turns on, or point the test at the old code path, and confirm it
goes red. State in the report which of the two you did.

## When the project has no harness at that level

A probe that needs an integration or browser harness the repo has never had is asking for
a bigger change than the one under review. Adding a test framework, a fixture layer and a
CI service container is its own PR, with its own review.

Say so, and offer the smaller thing instead: file it as a follow-up, and keep the probe in
the PR's verification section as a documented command someone can re-run by hand. That is
worth more than a half-built harness nobody maintains, and it leaves an honest record of
what is and is not covered.

## Clean up what you did not promote

Throwaway probes live in the scratch directory and stay there. A stray probe script in the
diff is noise a reviewer has to ask about, and a half-abandoned test file is worse — it
looks like coverage. Delete what you are not promoting, and say in the report where the
scratch directory is in case the user wants to re-run something by hand.

**Confirm the default selector actually collects it.** Every check up to here has run
the test *by name* — the baseline differential copies the file in and invokes it
directly, and so does the command you are about to quote. None of that proves the run
that matters picks it up. A repo whose runner globs `src/**/*.test.ts` will never collect
a file in `test/`, which is a location the table above offers by name; named explicitly
it passes, on every push it does not exist, and it reads as coverage forever — the exact
failure the fail-without-the-fix check is here to prevent. So run the project's
**unscoped** test command once and confirm the new test appears in the count. A tag that
excludes it from the default run (`@tag :integration` and friends) counts as not
collected: it may well be the right call, but say so rather than letting it pass as
coverage.

Promoted tests go on the branch under review, so the change and its guard land together.
Tell the user the path, the command that runs just that test, and the count that proved
the default run collects it.
