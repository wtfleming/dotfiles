# review-traps

A false-positive benchmark for `/wtf-code-review`. It answers one question about a
change to a rubric, a lens or the reviewer: did it make the review raise fewer things
that are not defects, without making it miss the ones that are?

Each case is a small change to the same toy app. A **trap** looks like a defect and is
not one: the guard is real, and it is somewhere a hasty reader does not look. A
**control** is a real defect of the same family. A change that lowers false positives on
the traps and starts missing the controls has only made the review quieter.

| Case | Ground | Why |
| ---- | ------ | --- |
| `sql-allowlisted-column` | safe | the concatenated column comes only from a fixed table; the value is bound |
| `guard-in-router` | safe | the handler has no check because the router wraps every admin route |
| `argv-clamped-int` | safe | the request value reaches a subprocess as a clamped integer in a fixed argv |
| `html-escaped-echo` | safe | both reflected values are escaped |
| `note-without-owner-check` | defect | a note is returned by id to any signed-in user |
| `sql-value-in-query` | defect | a request value is formatted into the SQL text |

`cases.tsv` is the ground truth: case, `safe` or `defect`, a pattern, and the reason.

## Running it

```sh
./run.sh all                      # every case, the full review
./run.sh all ~/traps/run-1 -- --lite   # keep the reports there, and pass --lite to the review
./run.sh score <case> <report>    # score a report you already have
```

Each run takes a directory of its own, outside any repository. `all` refuses one that
already holds a run, and refuses one inside a git work tree: `claude` reads `CLAUDE.md`
from every directory above where it runs, so a case built inside a checkout is reviewed
under that checkout's instructions, and here those say the fixtures are vulnerable on
purpose.

`all` spends model tokens — one review per case — so it is run by hand, before and
after the change being judged, and never in CI. It reviews with what is deployed in
`~/.claude`, so deploy the change first. Review output varies from run to run: compare
several runs a side, not one.

## Reading the result

A case is scored by searching the report's **Critical** and **Warning** sections for the
case's pattern. A match on a trap is a false positive; no match on a control is a miss.
A finding the report filed under Suggestion, or set aside as not worth acting on, does
not count, because the report did not ask for it to be fixed.

The pattern is a keyword match, so it is approximate both ways. It can count an
unrelated finding that happens to use the word, and it can miss the trap's finding
written in words the pattern lacks. Read the reports behind any number that will decide
something; `all` prints where they are. The case repos are numbered, not named, because
a case's name gives its answer away and the directory is in front of the reviewer;
`repos.txt` maps each number back to its case. A run that produced no report is counted as
that, never as clean.

## Adding a case

Add a directory under `cases/` holding only the files the change touches, laid over
`base/`, a row in `cases.tsv`, and a row in the table above. A trap needs its reason to be visible in the tree: a
case that is safe only because of something the reviewer cannot see measures nothing.

This directory is not deployed and holds deliberately vulnerable fixtures.

Adapted from the false-positive benchmark in Claude-BugHunter's `eval/` (MIT, see
`../../reference/LICENSE-bughunter`).
