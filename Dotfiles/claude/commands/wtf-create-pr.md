---
description: 'Compose and open a pull request — pre-flight the branch, report what has gone stale since it was written, then write a title that survives a squash merge and a body that is true in both directions. Use this whenever a finished branch is ready to go up for review: "open a PR", "raise a PR for this", "push this up for review", "PR this branch", "make a pull request". It composes and opens, and shows you both before it does; it does not review the code (that is /wtf-code-review), prove it works (wtf-code-verify), or merge anything.'
argument-hint: "[--draft] [extra context — an issue to close, framing the diff cannot show]"
allowed-tools: Read, Grep, Glob, Write, Bash(git:*), Bash(gh:*)
---

Arguments: $ARGUMENTS

Treat `--draft` as a flag and everything else as context for the body — an issue number,
a reason the diff cannot show, a reviewer's earlier ask. It may be empty, which is the
normal case: the branch is the input.

## What this does

Composes a pull request and opens it. That is the whole job. It does not review the code,
does not prove it runs, and does not merge — `/wtf-code-review` and `wtf-code-verify` own
the first two, and the third is a human decision made after CI reports.

The reason to keep that line sharp is that a PR-opening command is tempting to load up.
A version of this that also hunts for defects gets run at the wrong moment — early, while
the branch is still moving — and a version that gates on its own findings turns "open a
PR" into an argument. This runs when the branch is finished, reports what it found, and
opens.

## 1. Pre-flight

These checks are mechanical and will usually pass. Run them anyway, because each one that
fails fails quietly, and the failure is only visible after the PR is public.

**HEAD must not be the default branch.** Resolve the default per
`~/.claude/reference/scope-resolution.md` rather than assuming `main` — on a `master` or
`trunk` repo the assumption produces a base that looks resolved and is not. Standing on
the default branch there is nothing to open a PR *from*, so refuse and say what would fix
it. If the work is already committed there locally, moving it onto a branch rewrites local
history: propose the move and wait for a yes rather than performing it.

**The branch must be pushed and current with its remote.**

```sh
git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null   # is there an upstream
git status -sb                                                       # ahead / behind counts
```

No upstream means `git push -u`; ahead means `git push`. GitHub opens the PR against the
*remote* ref, so an unpushed commit is simply not in it — and the body you just composed
from the local diff describes code no reviewer can see. Behind or diverged is different:
say so and stop, because the fix is a rebase or a force-push and neither is yours to
choose.

**Check whether a PR already exists for this branch.**

```sh
gh pr list --head "$(git branch --show-current)" --state all --json number,url,state,isDraft
```

An open one means this is an update, not a create. Say so, keep the composition work, and
follow the rewrite rules in `~/.claude/reference/github-publishing.md` — carry across
everything that is not a description of the change, and replace generated sections rather
than stacking them — then `gh pr edit`. GitHub refuses a second open PR for the same
head and base, but it refuses with an error that reads as a failure rather than as *you
meant to edit*, and switching modes deliberately is better than reading that error and
guessing. A **closed** PR for the same branch is not a bar to opening a new one; mention
it, since reopening may be what was wanted.

**Name any uncommitted or untracked work, and ask.**

```sh
git status --porcelain
git ls-files --others --exclude-standard    # untracked, which diff never lists
```

None of it will be in the PR, and the author usually believes it will — a new source file
sitting beside a committed Markdown edit is the case that costs the most. List what is
there and ask whether to commit it or leave it behind. Do not decide silently in either
direction.

**Read the repo's own instructions before writing anything.** A
`.github/pull_request_template.md` (also `PULL_REQUEST_TEMPLATE.md`, a
`PULL_REQUEST_TEMPLATE/` directory of variants, or the repo-root and `docs/` spellings) is
filled in, not replaced. Its questions are what the reviewers here asked to be told, so a
body that answers a different set of questions reads as a bypass even when it is better
prose. `CONTRIBUTING.md` and any `CLAUDE.md` PR rules bind the same way — title format,
required sections, whether an issue link is mandatory.

## 2. What has gone stale

The checks above find something rarely. This section is where the findings actually live,
and all of it is about drift: things that were true when they were written and quietly
stopped being true while the branch moved. Judgement, not a checklist — three axes.

**Prose that describes the change.** Find the documentation that describes what this branch
changed — a README section, a CLAUDE.md rule, a doc comment above a function whose
contract moved — and check it still does. Apply the same two-direction test the body gets
below: every claim in it true, *and* every meaningful change accounted for. The second
direction is the one that finds things, because nothing in stale prose is false. It is
wrong by omission, and an omission does not announce itself: the paragraph reads correctly
right up to the sentence that should exist and does not.

**What was reviewed versus what you are opening.** If a review or a verification ran at
some commit and the branch has moved since, it covered code that no longer exists — while
the body is about to cite it as though it did. This is structurally the same failure as a
review bot reporting `pass` while its own status line reads `rate limited`: a review
genuinely happened, just not on this. Compare the SHA the review ran against to HEAD and
say plainly which commits it did not see.

**The verification artifact's freshness.** If `<scratch>/code-verify/VERIFICATION.md`
exists it records the SHA it covers. Compare that to HEAD. If they differ, the section
describes superseded code, and embedding it publishes a stale verdict under a heading that
reads as current. Two honest options: re-run the verification, or embed it with the SHA it
actually covers stated in the section itself.

**This is not a defect hunt.** `/wtf-code-review` owns that, and a readiness check that
starts finding bugs gets scheduled at the wrong point in the cycle and eventually skipped.
Nor is it a gate — "the bot has not reviewed the tip" is information the author needs, not
grounds to refuse. Report, then open.

**Come back empty, quickly, when nothing has drifted.** On a two-commit branch written in
one afternoon nothing has had time to go stale, and the honest output is a single line
saying so. A check that manufactures a finding to look like it earned its runtime is worse
than no check, because the next real finding arrives in the same voice as the invented one.

## 3. The title

On a squash merge the title becomes the permanent commit subject on the default branch,
where it outlives the PR, the branch and the review. Someone reading `git log` in a year
has the title and nothing else, so it has to stand alone away from the context that
explains it.

Judge the wording and any conventional-commit prefix separately. Where a repo derives a
changelog section or a release version from the prefix, a `fix:` that grew into a `feat:`
during the branch is not cosmetic — it is a wrong version bump. Match the repo's existing
style rather than importing one: read recent merged titles (`gh pr list --state merged
--limit 20 --json title`) and follow what is actually there, prefixes or not.

## 4. The body

Two directions, and it is only right when both hold:

- every claim in it is true of the code
- every meaningful change is accounted for

The second is the failure nobody catches. Nothing written is false, the description is
simply silent about a change that is in the diff, and silence does not read as an error —
so the reviewer forms a smaller picture of the change than the change actually is, and
reviews accordingly.

Carry across verbatim anything that is not a description of the change: `Closes #123`,
checklists, template sections, screenshots already present. Dropping the issue link
silently unlinks the issue and it will not close on merge. The full rule is in
`~/.claude/reference/github-publishing.md`, along with what belongs in a comment instead
of a body.

If `<scratch>/code-verify/VERIFICATION.md` exists from a `wtf-code-verify` run, include it
rather than writing a new one — subject to the freshness check above, and delimited per
the reference so a later run replaces it. **Never fabricate a verification section.** A
body claiming "tested manually" when nothing ran is precisely the defect `wtf-code-verify`
exists to catch, committed in the document that reports on it, where it is read as
evidence rather than as a claim.

## 5. Visual evidence

Attach before/after images or video when the change has a visual element *and* the
rendering carries information the text cannot. When the content is text — a JSON response,
a terminal transcript, a log — paste the text; the reference file says why a screenshot of
text is strictly worse.

Two gates before anything is attached:

1. **Is there a before?** A pair means something only against a prior state. A lone
   screenshot is a picture, not evidence. For a new capability, or any subject with no
   prior state, either skip it or say plainly that it shows only the new state.
2. **Is the repo private?** `gh repo view --json isPrivate`. Only private repos get visual
   evidence. GitHub serves private-repo attachments from
   `private-user-images.githubusercontent.com` behind a JWT, and public-repo attachments
   from `user-images.githubusercontent.com`, readable by anyone holding the link — and a
   screenshot of a running app carries real names, email addresses, avatars, a token in
   the URL bar, internal hostnames, none of which can be scrubbed by rule the way text
   can. On a public repo, describe the change in words and leave the images in the scratch
   directory. Worth stating once to the author: a private repo that later goes public
   exposes the attachments uploaded while it was private.

Then look at every image, per `~/.claude/reference/github-publishing.md`. Pixels have no
grep.

Mechanics: `gh pr create --attach './before.png#Alt text'`, repeatable, up to 50 files per
command, alt text after the `#`. Formats are PNG, JPEG, GIF, WebP, SVG, MP4, MOV and WebM.
Images are never the size constraint; video on a free plan is tight. Video renders as a
player and cannot take alt text, so describe it in the body instead.

Prefer SVG for anything generated — a chart, a diagram, a rendered plan. It is text, so
unlike a raster image it can be grepped for secrets before it goes up and diffed between
runs, and it is a fraction of the size.

Do not escalate work to produce a picture. If a run already had the browser open, the pair
costs seconds; opening one for the screenshot alone almost never pays.

## 6. Show it before it opens

Print the title, the body verbatim, the base and head refs the PR will use, any
attachments with their alt text, and whether it will be a draft. Then wait. This is the
last point at which a wrong title is free to fix — after opening, it is a public edit with
a notification behind it.

On a yes, write the body to `<scratch>/create-pr/body.md` and pass `--body-file`. A body
sent as a shell argument loses its formatting to quoting the moment it contains backticks
or blank lines, which is most bodies worth writing. Pass `--base` explicitly, resolved as
in the pre-flight, rather than trusting the repository default to be what this branch
targets.

## 7. After

Report the URL, and note two things about what happens next. CI is now running and has not
reported yet, so nothing here says the branch is green. And a green check from a review bot
can mean *rate limited, did not look* rather than *found nothing* — worth reading the
check's own output before treating it as a review.

Then stop. Do not merge, do not enable auto-merge, and do not request reviewers unless the
repo's conventions say to.

## Never

- Push to the default branch.
- Put AI or assistant attribution in the title, the body, or a commit message.
- Include a work email address, an internal hostname, or a credential — public repo, hard
  rule, and the reference file has the detail.
- Open the PR without showing the title and body first.
