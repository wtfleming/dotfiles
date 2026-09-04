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
~/.claude/scripts/resolve-scope.sh resolve [--scope <ref|range|path|PR#|PR-url>] [--base <ref>]
```

It prints the scope line and then the artifact directory, which holds two files:

- **`scope.diff`** — one diff covering the whole scope, untracked files included.
- **`manifest.json`** — what the diff is of, and how far it can be trusted.

Everything the rest of this file describes is what the script implements. Read on for why
each step is there and what to do with what comes back; do not re-derive the scope by hand
alongside it. **Several agents each deriving the scope separately is how "the same scope"
stops being true**, which is the entire reason the diff is an artifact rather than an
instruction.

One other subcommand: `base` prints the resolved default branch and nothing else, for
callers that need only that.

**Exit codes carry a decision.** `0` resolved. `2` means the scope is *prose* — a subject
naming an area of behaviour, which has no diff; take your subject procedure. `1` is a real
failure, and a single token that resolves to neither a ref nor a path is one of them: a
mistyped or unfetched branch name is a caller error, and routing it into the subject
procedure returns a confident review of a scope nobody asked for.

### Fields worth knowing by name

| Field | Use |
|---|---|
| `scope_line` | the whole Scope line, composed once, ready to paste into a report |
| `files` | repo-relative paths, projected from the diff itself |
| `correspondence`, `correspondence_note` | see below |
| `scope_head` | the commit the findings are about; `git show <scope_head>:<path>` reads it |
| `diff_path` | hand this to an agent instead of a description |
| `base_stale`, `base_stale_reason` | the fetch failed; the merge base may be behind. `base_stale` is always a boolean, including on the shapes that consult no base — only `base_stale_reason` is null. To ask whether a base was consulted at all, read `base_ref == null` or `default_branch_resolved`; `base_stale == false` means "no failed fetch", not "fetched and fresh" |
| `fell_through` | which auto steps produced nothing, and why |
| `file_list_source` | `git-apply-numstat` normally. `fallback-headers` means the list was scraped from the diff's headers and may be incomplete — **say so** rather than presenting `files` as settled |
| `warnings` | anything the resolver could not do cleanly; disclose these the way you disclose `base_stale` |

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

The candidate order is `origin/HEAD`, then `origin/main`, `origin/master`, `origin/trunk`,
then the bare `main`, `master`, `trunk` — and **every candidate is validated** with
`git rev-parse --verify` before it is used, including the one `origin/HEAD` names.
`resolve_default_branch()` in `~/.claude/scripts/resolve-scope.sh` is the implementation;
`resolve-scope.sh base` prints its answer. Stated here once as an order rather than as a
second copy of the loop, because two copies drift and only one of them runs.

Two things that order is doing deliberately. Remote-tracking refs come before local names
because they are the ones that exist in a clone nobody has branched in. And every candidate
is validated rather than trusted: `origin/HEAD` can be a dangling symbolic ref after the
upstream default branch is renamed, a local name can be missing in a fresh clone, and the
prefix can be stripped off a ref that needed it. All three produce a base that looks
resolved and is not.

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

The fetch is narrow (`git fetch <remote> <branch>`, not `--all`), **bounded** — five
seconds, because non-fatal is not the same as non-blocking and a remote that black-holes
SYNs would otherwise stall every review for the OS TCP timeout — and **non-fatal**:
reviewing offline is legitimate, so a failure is recorded in `base_stale_reason` and
disclosed rather than raised. It runs only for the shapes that actually consult a base: a
branch, and the auto fall-through's second step. A PR, a range, a single commit and a path
carry their own endpoints, so they neither resolve nor fetch one. Disclose it when it is set; the scope is still reviewable,
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

`correspondence` takes one of seven values, and `correspondence_note` carries the sentence to
disclose for each:

| Value | Meaning |
|---|---|
| `workspace` | the scope *is* the working tree; uncommitted state is the subject, not a contaminant |
| `same` | every tracked file matches the reviewed commit. Untracked files may still be present — they change no reviewed file, so reading from disk stays correct; `workspace_untracked` reports them |
| `same-dirty` | at the reviewed commit, but carrying uncommitted edits — a file on disk may not match the diff |
| `scope-behind` | the checkout is ahead of the review — reviewing `HEAD~3` |
| `scope-ahead` | the reviewed commits are not checked out — an unmerged branch |
| `divergent` | different lines of history — a fetched PR |
| `unknown` | the scope's head is not present locally, so no comparison was made |

`same-dirty` is a value of its own rather than a flag beside `same` because every agent's
rule keys on this one field. A checkout sitting at the reviewed commit with uncommitted
edits does not hold the reviewed code, and folding it into `same` leaves that state matching
no clause at all.

Direction is tracked because the two ancestor cases call for opposite disclosures, and
`unknown` is its own answer because reporting `divergent` for a comparison that was never
made is a claim nothing supports.

**None of these is a reason to abort.** Reviewing an older ref or a branch you would rather
not check out is a legitimate thing to ask for. What changes is how the code is read:

- On `workspace` and `same`, read files from disk as usual.
- On anything else — `same-dirty` included — **read the scope's blobs**:
  `git show <scope_head>:<path>`, not the working file. A lens told to read "the full
  current contents" of a file is being told to read the wrong file here. State the rule
  this way rather than listing the values: an enumeration has to be found and updated in
  every prompt when a value is added, and the one that gets missed leaves an agent with a
  state it has no instruction for.
- And in every non-`same` case: **a line that is not in the working tree is not a
  refutation.** It is the expected consequence of reading the wrong tree.

## State what you settled on

Whatever the procedure lands on, name it at the top of the report — use `scope_line`, which
already carries the ref, the file count, which of the three steps produced it, and the
correspondence note. A reader who cannot see the choice has no way to tell whether the
report covers the code they meant.
