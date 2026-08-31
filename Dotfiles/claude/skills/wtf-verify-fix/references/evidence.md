# Evidence and reporting

## Where things go

Everything lands in the session scratch directory, never in the repo — the probe is
not part of the change, and a stray probe file in a PR diff is noise a reviewer has
to ask about. The exception is a probe you promoted to a committed test (SKILL.md §4),
which belongs in the project beside its neighbours.

```
<scratch>/verify-fix/
├── probe.<ext>            # the probe itself, byte-identical for both runs
├── baseline.stdout.txt    # raw, verbatim, unsummarized
├── baseline.stderr.txt
├── head.stdout.txt
├── head.stderr.txt
├── control.txt            # the §6 control run
├── screenshots/           # tier 3, both sides, same viewport and path
└── VERIFICATION.md        # the section below, ready to paste
```

Capture raw output as it happened:

```bash
"$PROBE" > "$OUT/head.stdout.txt" 2> "$OUT/head.stderr.txt"; echo "exit=$?"
```

Capture both streams. Several test runners report results on stderr and leave stdout
empty — ERT is one — so a probe that captures only stdout looks like it produced
nothing at all, which is easy to misread as a crash. Keep the exit code too; it is
often the cleanest signal of the four.

Summarizing as you capture is how a `Cannot find module` in the baseline gets
recorded as "the expected failure." Write the bytes down first, read them second.

## The PR verification section

Paste into the PR description under the change summary. Keep it short — a reviewer
should be able to tell in fifteen seconds whether the claim was actually tested.

```markdown
## Verification

**Claim.** Before this change, `<input>` produced `<wrong observable>`. After, it produces `<right observable>`.

**Probe.** `<one line: what was run and what it observes>`
**Environment.** Tier `<0-3>` — `<test in package X | real render | booted service | browser>`
**Baseline.** `<short-sha>` (merge-base with `main`)

| Run | Result |
| --- | ------ |
| Baseline `<short-sha>` | ❌ `<the actual failure line>` |
| HEAD `<short-sha>` | ✅ `<the actual success line>` |
| Control (both) | ✅ `<what proved the baseline env was sound>` |

<details><summary>Raw output</summary>

​```
<baseline output, verbatim>
​```

​```
<head output, verbatim>
​```

</details>

**Not covered.** `<the branch, path, or condition the probe did not exercise>`
```

The control row is not decoration. Without it the baseline's ❌ is unfalsifiable — a
reviewer cannot tell a reproduced bug from a worktree that never built.

The "not covered" line is the most useful sentence in the section. Every probe is
narrow; naming the gap tells a reviewer where to look, and its absence invites them
to assume there isn't one.

## Variants

**Refactor** — the claim is that nothing changed, so the expected shape inverts and
the evidence is a diff:

```markdown
| Run | Result |
| --- | ------ |
| Baseline `<sha>` | ✅ output written to `baseline-out/` |
| HEAD `<sha>` | ✅ output written to `head-out/` |
| `diff -r baseline-out/ head-out/` | ✅ no differences |
```

Say what the input covered. An equivalence proof over trivial input proves the
trivial case.

**Performance** — a threshold and a distribution, never a single pair of numbers:

```markdown
| Run | Median | Range (n=5) |
| --- | ------ | ----------- |
| Baseline `<sha>` | 1240 ms | 1190–1310 |
| HEAD `<sha>` | 890 ms | 860–930 |

Threshold set before measuring: ≥15% reduction. Met.
```

If the ranges overlap, the honest report says the runs did not separate — not the
better median with the spread omitted.

**Flagged** — off must match the baseline exactly, which is the claim a flag makes:

```markdown
| Run | Result |
| --- | ------ |
| HEAD, flag off | ✅ matches baseline |
| HEAD, flag on | ✅ `<new behaviour>` |
```

## Terminal report

Lead with the verdict in one line, then the evidence, then the path to the files.

> Verified. Baseline `2427dfb` throws `TypeError: cannot read 'slug' of undefined` on
> the dynamic page; HEAD renders all 12 pages. Control (the static pages) passed on
> both. Files in `<scratch>/verify-fix/`, PR section in `VERIFICATION.md`.

When it did not verify, the verdict line says so first and names which of the §8 rows
you landed on. A "not verified" that arrives at the end of three paragraphs of
process gets read as a success.
