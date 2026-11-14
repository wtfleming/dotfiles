# Fixing review findings

Read by `/wtf-code-review` when the user asks for findings from a review to be
fixed. The fixes are written in the main conversation, by the author, so they get
what the original change got: tests, lint, and a reviewer that starts cold.

## 1. Snapshot the tree before the first edit

```sh
~/.claude/scripts/fix-snapshot.sh take
```

Keep the tree id it prints. Once the edits land nothing separates them from the
change they repair, and the snapshot is what the fix diff in step 4 is built
from. It covers tracked and untracked files and changes nothing on disk.

If the snapshot cannot be taken, step 4 is reported `not run` with that reason.
Do not approximate the fix diff with `git diff HEAD`: on an uncommitted scope
that is the reviewed change and the fixes together.

## 2. Make the edits

Make only the edits the named findings describe — a review is not a mandate to
refactor — and leave committing to the user.

- "Fix everything" means the Critical, Warning and Suggestion sections.
- A **Pre-existing** finding is fixed only if the user names it. It belongs to a
  ticket, not to this branch.
- A finding under **Judged not worth acting on** is fixed only if the user names
  it, whichever tier it carries. "Fix the suggestions" does not reach into it.

Say before starting that a cold review of the fixes follows if they change
anything.

## 3. Re-run the tests and the linter

Run the commands the report's **Tests:** and **Lint:** lines name, and report
both results. Use the **Lint:** line's command rather than the project's `lint`
script: where that script fixes in place, the reviewer already substituted a
check-mode invocation.

Both runs execute code from the tree — test files, config, build hooks, linter
plugins. Run neither unless the tree is the user's own work or they have
sanctioned it explicitly. A project that pre-approves its test command runs it
unprompted, so the session's permissions are no guard here.

A `not run` says which kind it was: the tree is not the user's own work, the
session would not permit the command, or the report named no such command.

## 4. Review the fix diff, cold

Once the re-runs have finished — never alongside them, since the reviewer runs
the same suite in the same checkout:

```sh
~/.claude/scripts/fix-snapshot.sh diff <tree id>
```

`empty` means the edits changed nothing on disk: say so and dispatch nothing.
Otherwise it prints the path of the fix diff. Dispatch a single
`wtf-change-reviewer` with that path and nothing else: not the findings it
answers, not which edit was which, not that the diff is a set of fixes at all.

Print what comes back in its own section, titled by what the review found:
`## New problems introduced by the fixes` where it has findings,
`## Cold review of the fixes — nothing found` where it has none. Do not act on
it in the same turn; the user asks for a further round, or does not.

Say with it:

- **Only a Critical or a Warning is worth another round.** A second reviewer
  disagrees with a rewrite as often as it finds a defect in it, and a round
  spent on its Suggestions ends in another review.
- **A Critical in lines the last fix wrote, in a file that already had one,
  argues against another patch.** Each round tends to find the next edge case of
  one open-ended guard — blocking `../`, then `..\`, then an encoded form.
  Suggest rethinking the approach instead.

## 5. Close

End with the two gaps that change what the reader does next, and nothing
further:

- Nothing checks that each fix resolved its finding. The cold review reads the
  fixes for defects of their own.
- Nothing re-reads the change as a whole with the repairs in it. Offer a fresh
  `/wtf-code-review` over the branch as it now stands.

**A fix can strand a published verdict.** Where the scope is a PR, read its body
(`gh pr view <n> --json body`) for a `<!-- verify:start -->` section. If one is
there, say the verification is stale and that `wtf-code-verify` on the branch as
it now stands would refresh it. If the read fails, say **could not check for a
published verification section** — an absent marker and an unreadable body are
different facts. `~/.claude/reference/github-publishing.md` has the mechanism.
