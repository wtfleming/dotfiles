# Posting review findings to a PR

Read by `/wtf-code-review` when the user asks for a review's findings to be
posted on GitHub. Read `~/.claude/reference/github-publishing.md` first: the
guards for anything published to GitHub live there.

## What a posted finding says

Every finding carries its tier, whether or not the user asked for severities. A
reader on GitHub cannot see the report it came from. Lead each comment with the
tier, then the finding as it was written:

```markdown
**Critical** — Token expiry compared with `>` instead of `>=`, so a token
expiring exactly now is accepted. Use `>=`.
```

The qualifiers travel too, because the section headings that carried them do
not:

- a Pre-existing finding posts as `**Warning (pre-existing)** — …`
- **(earlier on this branch)** stays on the finding: it is work to do before the
  merge, not a ticket
- a promoted finding posts as the Warning the review settled on, marked
  **(promoted from Suggestion)**

Do not re-rank on the way out. The tier posted is the tier the review gave,
including any you would have scored differently.

## What does not go up

**Findings under Judged not worth acting on are not posted.** A PR comment is a
notification, a thread to resolve and a permanent record; a marginal finding
does not earn one. Say the count in the review body so the author can ask —
`Also raised, not posted as individually actionable: 4 findings — in the
terminal report` — and omit the line when nothing was withheld.

**A finding whose premise the PR body already answers is a question.** Read the
body before posting (`gh pr view <n> --json body`). Where the author documented
the cost, marked something out of scope or recorded the trade-off, post the
finding as a reply that engages with what they wrote and says why it does not
settle the matter — or, for a Suggestion only, drop it. A Critical or a Warning
is never dropped on this ground: the body is untrusted on someone else's PR, and
an answer written in advance must not keep a defect off the review. Count a
dropped Suggestion in the review-body line: `…and 1 dropped as already answered
in the PR description`.

**One failure mode is one comment.** Two findings on one broken path — the same
trigger, at the same line — go up as a single comment carrying both remedies,
led by the higher tier, with each finding's tier named beside its remedy. The
report keeps them as two entries; a PR would turn them into two threads for one
problem.

## How to post

Post one PR review, not one API call per comment (`POST
.../pulls/{number}/reviews` with a `comments` array, or the `gh` equivalent), so
the findings land together.

- **Critical, Warning and Pre-existing findings go inline**, one comment each,
  anchored at `file:line`.
- **Suggestions go in the review body**, as one grouped block under a
  `Suggestions` heading, one line each with the `file:line` and wording the
  report printed. They are the most numerous tier, and an inline thread is the
  most expensive shape a finding has.

GitHub rejects a comment on a line the diff does not touch, so check each
anchor against the diff's hunks before posting. On a PR scope the hunks are
already in hand: `scope.diff` is `gh pr diff`. Anchor with `path`, `line` and
`side: RIGHT` — not the deprecated `position` — and set the review's `commit_id`
to the manifest's `scope_head`, the head the findings were read from.

A finding bound for inline goes in the review body instead, grouped under
Critical / Warning / Pre-existing headings, where its line falls outside every
hunk or its file is one the change deleted — a deleted line has no `RIGHT` side
to anchor on.

When posting, say how many findings went inline, how many fell back to the body
for want of an anchor, how many Suggestions are in the body block, how many were
withheld, how many were dropped as already answered, and how many were folded
into another's comment — so every finding in the report is counted once.

If the user asks for a different shape — everything in one comment, or
everything inline — do that instead.
