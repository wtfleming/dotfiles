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
"$PROBE" > "$OUT/raw/p2.head.stdout.txt" 2> "$OUT/raw/p2.head.stderr.txt"; echo "exit=$?"
```

Capture both streams and the exit code. Several test runners report results on stderr and
leave stdout empty — ERT is one — so a probe that captures only stdout looks like it
produced nothing at all, which is easy to misread as a crash. The exit code is often the
cleanest signal of the three.

Summarizing as you capture is how a `Cannot find module` gets recorded as "the expected
failure". Write the bytes down first, read them second.

## The PR verification section

Paste under the change summary. Keep it short — a reviewer should be able to tell in
fifteen seconds whether the claim was actually tested, and by what.

````markdown
## Verification

**Verdict.** Verified with gaps — 3 of 4 expectations met, 1 defect found and fixed in `a91c` (see below).

**Scope.** `feat/archived-posts` vs merge-base `2427dfb` — 4 files
**Environment.** Tier 2 — service from the worktree, compose dependencies (`db`, `redis`)

| # | Expectation | Discriminator | Result |
| - | ----------- | ------------- | ------ |
| 1 | editor with `includeArchived: true` sees archived id 7 | flag off → id 7 absent | ✅ 200, id 7 present |
| 2 | anonymous caller is refused | valid session → 200 | ✅ 401 `UNAUTHENTICATED`, no `data.posts` |
| 3 | `includeArchived: "banana"` → 400, field-level error | valid value → 200 | ❌ 500 `Boolean cannot represent a non boolean value` — fixed in `a91c`, now 400 |
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

**Scrub the outward copy.** The capture is deliberately unfiltered, and the worktree it
came from has the machine's real `.env` symlinked in — so the raw bytes can carry an
`Authorization: Bearer …`, a `postgres://user:password@host`, an API key echoed by a
verbose client, or a DSN printed by a failed connect. A GitHub comment is indexed and
mirrored to notification email, so editing it later does not take it back. The reviewer
agent next door already works this way for the read-only side: cite a key by name, never
by value. Quote only the assertion-bearing lines in the posted copy, replace any
credential with `<redacted: SESSION_TOKEN>`, and leave the unabridged capture in the
scratch directory where it belongs. Filtering at *capture* time is still wrong — that is
what launders a compile failure into "the expected failure" — which is exactly why the
scrub goes here instead.

```bash
gh pr comment <n> --body-file VERIFICATION.md      # a comment, reversible

# Into the body instead. The fetch alone changes nothing on GitHub, so all three steps
# are needed — and the section is delimited so a rerun *replaces* it rather than stacking
# a second one below the first. Two verdicts in one body, oldest first, is how a stale
# "Verified" outlives the run that retracted it.
gh pr view <n> --json body -q .body \
  | awk '/<!-- verify:start -->/{skip=1} !skip; /<!-- verify:end -->/{skip=0}' > "$OUT/body.md"
{ echo '<!-- verify:start -->'; cat "$OUT/VERIFICATION.md"; echo '<!-- verify:end -->'; } >> "$OUT/body.md"
gh pr edit <n> --body-file "$OUT/body.md"
```

Prefer a comment unless the user asks for the body. A comment is timestamped, attributable
and easy to supersede when a later run changes the verdict; an edited body silently
replaces whatever was there, including someone else's text.

Where the description drifted, propose the new title and body, show both, and ask before
`gh pr edit`. Carry across everything that is not a description of the change —
`Closes #123`, checklists, screenshots, template sections — because a regenerated body
that loses the issue link silently stops it closing on merge. On someone else's PR,
report the drift and stop; the wording is theirs to fix.

Re-read the verdict line before posting. Verification sections are read as endorsements,
and a "verified with gaps" whose gaps are buried below a fold reads as an unqualified
pass.

## The terminal report

Lead with the verdict in one line, then the evidence, then the path to the files.

> **Verified with gaps.** 3 of 4 expectations met; probe 3 found a real defect — the
> coercion error surfaced as a 500 rather than a 400 — fixed in `a91c` and re-run green.
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
