# Evidence and reporting

## Contents

- Where things go
- Capturing output
- The PR verification section
- Variants: refactor, performance, flagged
- Posting it to the PR
- The terminal report

## Where things go

Everything lands in the session scratch directory, never in the repo — a probe is not
part of the change, and a stray probe file in a PR diff is noise a reviewer has to ask
about. The exception is a probe promoted to a committed test (`promotion.md`), which
belongs in the project beside its neighbours.

```
<scratch>/code-verify/
├── plan.md                # the expectations, written before anything ran
├── probes/                # the probes themselves, byte-identical across sides
├── raw/
│   ├── <probe>.baseline.stdout.txt    # raw, verbatim, unsummarized
│   ├── <probe>.baseline.stderr.txt
│   ├── <probe>.head.stdout.txt
│   ├── <probe>.head.stderr.txt
│   └── control.txt                    # what proved the environment was sound
├── screenshots/           # tier 3, both sides, same viewport and path
└── VERIFICATION.md        # the section below, ready to paste
```

`plan.md` matters more than it looks. It is written before execution and not edited
afterwards, so it is the only artifact that can show the expectations were not reverse
engineered from the results. If a prediction turns out to have been wrong about what
correct means, add a line saying so rather than editing the original.

## Capturing output

```bash
status=0
"$PROBE" > "$OUT/raw/p2.head.stdout.txt" 2> "$OUT/raw/p2.head.stderr.txt" || status=$?
echo "$status" > "$OUT/raw/p2.head.exit"
```

Into a variable first, then the file. `$?` survives exactly one command, so anything that
records it also destroys it: `; echo "exit=$?"` *prints* the status and keeps nothing, and
`echo $? > file` writes it and leaves `$?` as `echo`'s own zero — so a probe that failed
reads as one that passed to whatever runs next. The `|| status=$?` form matters under
`set -e`, where a failing probe would otherwise abort before anything could record it.

A probe wrapped in a script of its own has to **return** that status too — `exit "$status"`
as its last line, not the status of the last thing it wrote.

Capture both streams and the exit code. Several test runners report results on stderr and
leave stdout empty — ERT is one — so a probe that captures only stdout looks like it
produced nothing at all, which is easy to misread as a crash. The exit code is often the
cleanest signal of the three.

Summarizing as you capture is how a `Cannot find module` gets recorded as "the expected
failure". Write the bytes down first, read them second.

## The PR verification section

Paste under the change summary. Keep it short — a reviewer should be able to tell in
fifteen seconds whether the claim was actually tested, and by what.

**It describes the code, not the run that got there.** Every row states what the code in
this PR does now. A defect this run found and fixed does not appear: the fix is a commit
in the branch, so the diff already carries it, and narrating it here duplicates what the
reviewer is about to read while leaving them to work out whether the ❌ still applies.

The test is which before-and-after a sentence is about. Against the base branch it is the
change itself and belongs here — "the resolver now returns 400" is what the PR is for.
Against an earlier state of this same branch it is the run's history, and a row carrying
an ❌ for code that no longer exists is the clearest case: it reads as a defect the
reviewer should look for.

That history is not thrown away, it is addressed to someone else. The author gets it in
the terminal report, where a defect caught before the merge is the most valuable thing
the run produced; the reviewer gets the state of the code they are being asked to
approve. The one exception is a claim still unproven at the end — a gap, an environmental
failure, a non-deterministic probe — which is a fact about this code and belongs here.

````markdown
## Verification

**Verdict.** Verified with gaps — 4 of 4 expectations met against this code; one path went unexercised (see **Not covered**).

**Scope.** `feat/archived-posts` vs merge-base `2427dfb` — 4 files
**Verified at.** `5544ef1` — the commit the probes actually ran against
**Environment.** Tier 2 — service from the worktree, compose dependencies (`db`, `redis`)

| # | Expectation | Discriminator | Result |
| - | ----------- | ------------- | ------ |
| 1 | editor with `includeArchived: true` sees archived id 7 | flag off → id 7 absent | ✅ 200, id 7 present |
| 2 | anonymous caller is refused | valid session → 200 | ✅ 401 `UNAUTHENTICATED`, no `data.posts` |
| 3 | `includeArchived: "banana"` → 400, field-level error | valid value → 200 | ✅ 400 `BAD_USER_INPUT`, error names the field |
| 4 | `posts` with no new argument is unchanged | baseline `2427dfb` | ✅ byte-identical response |

<details><summary>Raw output</summary>

```
<redacted, assertion-bearing excerpts only, per probe, both sides —
the unabridged capture stays in <scratch>/code-verify/raw/>
```

</details>

**Covered.** The resolver's authorisation branch and argument coercion.
**Not covered.** The admin override path — needs a second seeded role. The subscription
resolver shares the same guard and this PR does not touch it.
**CI.** Unit suite and lint run on every push; none of the four above is in CI today.
**Residue.** None — compose dependencies down, worktree removed.
````

The **Discriminator** column is the part that cannot be omitted. It is what separates a
result from a result that means something: it says how you know each ✅ would have been a
❌ if the code were wrong. A table without it is a list of things that happened.

The **Not covered** line is the most useful sentence in the section. Every probe is
narrow; naming the gap tells a reviewer where to look, and its absence invites them to
assume there isn't one.

**Verified at** is `git rev-parse --short HEAD`, taken when the probes run — not the merge
base, which **Scope** already carries and which is a different commit for a different
purpose. The short form is deliberate and so is stating it: `/wtf-create-pr` compares this
field against HEAD, and comparing an abbreviated SHA to a full one is unequal on every
commit, which would age every fresh artifact as stale. Both sides abbreviate
(`git rev-parse --short HEAD`), and a reader who wants the full hash has the branch.
Anything that reads this section later has to answer "does this still describe the code?",
and only a head SHA answers it: `/wtf-create-pr` compares this field to HEAD before
embedding the section in a PR body, and without it a stale verdict cannot be told from a
current one. Where the tree was dirty when the probes ran, say so — `` `5544ef1` + uncommitted
edits`` — because the run then corresponds to no commit anyone else can check out, which a
bare SHA would hide.

For a single-claim change a one-row table is fine, but keep all four trailing lines. They
are short, and each one is a question a reviewer would otherwise have to ask.

## Variants

**Refactor** — the claim is that nothing changed, so the evidence is a diff:

```markdown
| # | Expectation | Discriminator | Result |
| 1 | same input, byte-identical output | baseline `2427dfb`, same 340-record fixture | ✅ `diff -r baseline-out/ head-out/` empty |
```

Say what the input covered. An equivalence proof over trivial input proves the trivial
case.

**Performance** — a threshold and a distribution, never a single pair of numbers:

```markdown
| Run | Median | Range (n=5) |
| --- | ------ | ----------- |
| Baseline `<sha>` | 1240 ms | 1190–1310 |
| HEAD `<sha>` | 890 ms | 860–930 |

Threshold set before measuring: ≥15% reduction. Met.
```

If the ranges overlap, the honest report says the runs did not separate — not the better
median with the spread omitted.

**Flagged** — off must match the baseline exactly, which is the claim a flag makes:

```markdown
| # | Expectation | Discriminator | Result |
| 1 | flag off behaves as before | baseline `<sha>` | ✅ identical |
| 2 | flag on enables `<behaviour>` | flag off → old behaviour | ✅ `<observable>` |
```

## Posting it to the PR

Offer; do not post unasked. The section is public, it carries a verdict in the user's
name, and one landing on the wrong PR is worse than none.

**The guards on anything going to GitHub live in
`~/.claude/reference/github-publishing.md`** — scrubbing the text, delimiting a generated
section so a rerun replaces it rather than stacking a second verdict below the first, and
what belongs in a comment instead of a body. Read it before posting. Two things it says
land hardest here: the capture this section quotes came from a worktree with the machine's
real `.env` symlinked in, so assume the raw bytes carry a live credential; and filtering
at *capture* time is still wrong — that is what launders a compile failure into "the
expected failure" — which is why the scrub happens at this step and not the earlier one.

**The body is the default channel**, for the reasons the reference gives under **Who may
write what**. The merge is a script, not a recipe to retype —
`~/.claude/scripts/publish-verify-section.sh merge <body> VERIFICATION.md <out>`, wrapped in
the read and lost-update guards the reference shows. It replaces the section where it sits,
leaves a marker the author only quoted alone, and refuses any merge that would touch a byte
outside the section.

**Establish whose PR it is before choosing the channel.** `gh pr view <n> --json
author,headRepositoryOwner` — the field `/wtf-create-pr` already keys on for its own
purposes. Nothing else in a verify run reads it: the drift check fetches
`title,body,createdAt,commits,reviews,comments` and the resolver captures only
`headRefOid`, so the manifest carries no ownership fact, and the execution gate never fires
on a prose-claim run that needs no boot. Without the check the default channel is a
read-modify-write over a contributor's own description. Say whose PR it is in the offer.

```bash
gh pr comment <n> --body-file VERIFICATION.md      # the fallback, and reversible
```

Fall back to a comment where the body is not available to write or not the right place for
it: someone else's PR, a body the merge refused (doubled or out-of-order markers, or a
change that would reach outside the section), a tripped lost-update check, or a user who
asks for a comment. Say which channel was used, since the two are read differently — a
comment is timestamped and attributable, a body section reads as current.

**On a refusal, name the state you left behind.** Every refusal except "the user asked for a
comment" means the live body is *already* wrong — a half-section, two verdicts oldest-first,
or a section the merge would not touch — and the new verdict has just gone somewhere else. A
reviewer opening that PR reads the body's older verdict as current, and the guard's own
message went to stderr where nobody sees it. So report that the body still holds an
unreconciled `verify:start` section that reads as current and needs a hand fix, alongside
which channel was used. A run that recovered itself and left the PR misleading has not
finished.

**Where the description drifted, report it and hand off.** Name what drifted — the claim
in the title or body, and the code that contradicts it — and point at `/wtf-create-pr`,
whose update path routes an existing PR to `gh pr edit` and carries the survivors across.
Do not propose replacement wording here. On someone else's PR the wording is theirs to
fix; on your own it is one command away, and the drift you found is what that command
needs to hear.

Re-read the verdict line before posting. Verification sections are read as endorsements,
and a "verified with gaps" whose gaps are buried below a fold reads as an unqualified
pass.

## The terminal report

Lead with the verdict in one line, then the evidence, then the path to the files.

> **Verified with gaps.** 4 of 4 met after the fix; probe 3 initially returned a 500 rather
> than a 400 — a real defect, fixed in `a91c` and re-run green.
> The admin override path went unexercised. Files in `<scratch>/code-verify/`, PR section
> in `VERIFICATION.md`.

When it did not verify, the verdict line says so first and names which case it was: green
on the base too, red for environmental reasons, or non-deterministic. A "not verified"
that arrives after three paragraphs of process gets read as a success.

## Nothing in the report is asserted from memory

The evidence table is safe by construction — each row has a probe and a discriminator
behind it. The four trailing lines are not, because no probe produces them, and they are
the ones a reader most relies on. Each has a command that settles it:

| Line | What establishes it |
| ---- | ------------------- |
| **Covered / Not covered** | the probes actually run, listed from the scratch directory — not the expectation list, which is what you intended to run |
| **CI** | reading `.github/workflows` or equivalent, this run, not from memory of the repo |
| **Residue** | `git status --porcelain`, `docker compose ps`, `git worktree list`, and the scratch path — checked after teardown, not predicted before it |
| **PR description** | the body re-read at the end, since your own commits may have outdated it since you looked |

Counts and figures belong to the same rule. A character count taken from raw text rather
than from a parse, a row count guessed from a fixture, a timing quoted from one run and
reported as typical — each reads exactly like a measured value and is not one. If a figure
is in the report, the thing that produced it should be re-runnable.

Where a check was not possible, the honest form is one clause: *assumed, not checked*.
That costs a reader nothing and tells them precisely which sentence to distrust.
