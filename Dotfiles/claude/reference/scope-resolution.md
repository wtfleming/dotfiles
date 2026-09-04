# Resolving a scope

Every review, verification and design pass has to answer two questions before it reads a
line: **what code is under review**, and **does the working tree actually hold it**.

Getting either wrong is quiet rather than loud. The git commands below can return an empty
result and exit 0, and an empty result read as "no changes" produces a confident report
about nothing. A tree that does not match the scope is worse, because it fails in one
direction only — see *Correspondence* below.

This file is the single statement of the procedure. `wtf-code-verify`, `wtf-change-reviewer`,
`wtf-design-reviewer`, `wtf-code-review`, `/wtf-code-review-no-lenses` and `/wtf-create-pr`
all point here rather than restating it, so a fix lands once.

## Run the script

```sh
~/.claude/scripts/resolve-scope.sh resolve [--scope <ref|range|path|PR#>] [--base <ref>] [--no-fetch]
```

It prints the scope line and then the artifact directory, which holds two files:

- **`scope.diff`** — one diff covering the whole scope, untracked files included.
- **`manifest.json`** — what the diff is of, and how far it can be trusted.

Everything the rest of this file describes is what the script implements. Read on for why
each step is there and what to do with what comes back; do not re-derive the scope by hand
alongside it. **Several agents each deriving the scope separately is how "the same scope"
stops being true**, which is the entire reason the diff is an artifact rather than an
instruction.

Two other subcommands: `base` prints the resolved default branch and nothing else, for
callers that need only that; `path` prints the artifact directory for a scope without
resolving it, so two callers naming the same scope agree on where its artifacts are.

### Fields worth knowing by name

| Field | Use |
|---|---|
| `scope_line` | the whole Scope line, composed once, ready to paste into a report |
| `files` | repo-relative paths, projected from the diff itself |
| `correspondence`, `correspondence_note` | see below |
| `scope_head` | the commit the findings are about; `git show <scope_head>:<path>` reads it |
| `diff_path` | hand this to an agent instead of a description |
| `base_stale`, `base_stale_reason` | the fetch failed; the merge base may be behind |
| `fell_through` | which auto steps produced nothing, and why |

`scope_line` exists so that a reviewer, eight lenses and a merged report describe one scope
in one form rather than nine. Use it rather than composing your own.

## The order, when nothing is named

1. **Uncommitted work**, if there is any:

   ```sh
   git status --porcelain
   git diff                                    # unstaged
   git diff --staged                           # staged
   git ls-files --others --exclude-standard    # untracked, which diff never lists
   ```

   The last one matters. A new source file beside a Markdown edit is exactly the change
   that would otherwise pass as prose, and `git diff` does not mention it. **Untracked
   files belong in the same diff as everything else** — the script folds them in with
   `git diff --no-index /dev/null <file>`, which produces a real `new file mode` hunk and
   reduces a binary to a single line. Listing them separately leaves each agent to
   rediscover the untracked half, or not.

2. **The branch against its merge base**, if the tree is clean.

3. **`git show HEAD`**, if that is empty too.

## An empty diff means fall through, not "no changes"

Two ordinary situations produce an empty merge-base diff with exit 0: standing on the
default branch itself, and the collapsed substitution described below. Neither means the
branch has no changes. Treat empty as *this step found nothing, continue to the next* —
never as a verified scope of zero.

The script enforces this rather than asking for it: **if `manifest.json` exists, the scope
is non-empty.** Every path that would describe an empty scope either falls through to the
next step or exits without writing anything, and the steps that produced nothing are
recorded in `fell_through` so a report can say which of the three settled it. There is no
way to be handed an artifact that claims an empty scope, because a collapsed range and a
genuinely empty change look identical on disk.

## Resolve the default branch; do not hardcode `main`

```sh
# Take the first candidate that resolves to a commit. Validating the result rather than
# trusting the source is the whole point: origin/HEAD can be a dangling symbolic ref
# after the upstream default branch is renamed, a local name can be missing in a fresh
# clone, and the prefix can be stripped off a ref that needed it. All three produce a
# base that looks resolved and is not.
base=""
for c in "$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true)" \
         origin/main origin/master origin/trunk main master trunk; do
  [ -n "$c" ] || continue
  if git rev-parse --verify --quiet "$c^{commit}" >/dev/null 2>&1; then base=$c; break; fi
done
[ -n "$base" ] || echo "cannot resolve a default branch; ask for one" >&2
```

Two things that snippet is doing deliberately. Remote-tracking refs come before local
names because they are the ones that exist in a clone nobody has branched in. And every
candidate goes through `rev-parse --verify` including the one `origin/HEAD` names — a
symbolic ref is a pointer, and nothing guarantees its target still exists.

Note the `if … then … fi` rather than `… && base=$c && break`. The latter is a statement
whose value is 1 when a candidate misses, so under `set -e` it kills the script on the
first non-resolving candidate — in exactly the repos the loop exists for.

On a `master` repo, `git merge-base HEAD main` fails, a `$(...)` substitution collapses to
empty, and `git diff ...HEAD` degrades to `HEAD...HEAD` — empty output, exit 0, no stderr
anyone reads. That is the failure this section exists to stop, and it looks identical to a
clean tree.

If none of them resolves — `origin/HEAD` unset and the default branch named something else
— **say so and ask, rather than guessing.** Falling through to `git show HEAD` there
reviews a single commit of a branch that has many, and reports it as the whole scope.

## Fetch the base before computing a merge base

A stale remote-tracking ref does not fail. It moves the merge base backwards, and the
review scope silently grows to include commits nobody asked to review — on a long-lived
branch, other people's commits, reported as this branch's changes.

The fetch is narrow (`git fetch <remote> <branch>`, not `--all`) and **non-fatal**:
reviewing offline is legitimate, so a failure is recorded in `base_stale_reason` and
disclosed rather than raised. Disclose it when it is set; the scope is still reviewable,
it just may be wider than the branch.

A base that resolved to a *local* name is not stale — a local name is only ever chosen
when no remote-tracking ref resolved, so there is no remote for it to be behind. Reporting
staleness there would put a warning on every review in a local-only repo, which spends the
credibility of the real one.

## On a PR, `gh pr diff` is the authority

Never substitute a locally computed `git diff <base>...<head>` for a pull request. The
local base ref may be stale, the PR may target a non-default base, and the PR may have been
rebased since it was opened; merge-base and ref timing diverge, and that divergence *is*
the "the diff doesn't match what GitHub shows" failure class.

**A failed `gh pr diff` is a hard stop, not a fallback.** Silently reviewing a near-miss of
the PR is worse than not reviewing it, because the report claims to be about the PR.

This also settles the anchoring problem downstream: when findings go back to GitHub as
inline comments they are checked against `gh pr diff` hunks, and if the review itself read
something computed a different way, the two artifacts disagree about which lines exist.
Reviewing the PR diff makes the anchor check and the review about one artifact.

## Correspondence: does the tree hold the code under review?

Reviewing `HEAD~3`, or a branch that is not checked out, is ordinary — and in both, the
files on disk are **not** the files under review. An agent that reads the working tree
there judges the wrong code.

This fails in one direction only, which is why it earns a field of its own. `wtf-refuter`
reads the working tree unless told otherwise and answers `refuted` when it cannot decide.
A refuter pointed at the wrong tree does not find the line a finding names, cannot decide,
and kills it. So a mismatch does not add noise — **it silently deletes true findings.**

`correspondence` takes one of six values, and `correspondence_note` carries the sentence to
disclose for each:

| Value | Meaning |
|---|---|
| `workspace` | the scope *is* the working tree; uncommitted state is the subject, not a contaminant |
| `same` | the checkout is at the reviewed commit |
| `scope-behind` | the checkout is ahead of the review — reviewing `HEAD~3` |
| `scope-ahead` | the reviewed commits are not checked out — an unmerged branch |
| `divergent` | different lines of history — a fetched PR |
| `unknown` | the scope's head is not present locally, so no comparison was made |

Direction is tracked because the two ancestor cases call for opposite disclosures, and
`unknown` is its own answer because reporting `divergent` for a comparison that was never
made is a claim nothing supports.

**None of these is a reason to abort.** Reviewing an older ref or a branch you would rather
not check out is a legitimate thing to ask for. What changes is how the code is read:

- On `workspace` and a clean `same`, read files from disk as usual.
- On anything else, **read the scope's blobs**: `git show <scope_head>:<path>`, not the
  working file. A lens told to read "the full current contents" of a file is being told to
  read the wrong file here.
- And in every non-`same` case: **a line that is not in the working tree is not a
  refutation.** It is the expected consequence of reading the wrong tree.

## State what you settled on

Whatever the procedure lands on, name it at the top of the report — use `scope_line`, which
already carries the ref, the file count, which of the three steps produced it, and the
correspondence note. A reader who cannot see the choice has no way to tell whether the
report covers the code they meant.
