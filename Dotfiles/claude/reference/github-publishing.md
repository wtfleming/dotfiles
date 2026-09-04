# Publishing to GitHub

Three tools here write outward: `/wtf-code-review` posts findings, `wtf-code-verify` posts
verification sections, `/wtf-create-pr` composes the pull request itself. What they publish
leaves your control the moment it lands — GitHub indexes it, mirrors it into notification
email, and serves it to anyone who can read the repo. Editing a comment afterwards does not
take back what was already delivered.

This file is the single statement of the guards that apply to all three. Each one points here
rather than restating them, so a fix lands once. Nothing below is about *what* to say; it is
about who may write where, and the ways an otherwise correct message goes wrong on the way
out. No count, so the next addition does not have to edit this sentence.

## Who may write what

A PR has **three overwritable places** — the title, the author's prose in the body, and a
delimited generated section in the body — and they are owned separately because a rewrite
replaces rather than appends and `gh pr edit` has no compare-and-swap (see below), so what
a tool may overwrite has to be text it wrote itself. Comments are the fourth place to write
and are not on this list because nothing overwrites them; they are governed by **Put what
cannot be regenerated in a comment** below, and are always open to a tool that owns no
section.

- **The title** — `/wtf-create-pr` only. It becomes the permanent commit subject on a
  squash merge, there is nowhere in it to delimit a generated section, and so a write is
  replace-or-nothing on a line that outlives the PR.
- **The author's prose in the body** — `/wtf-create-pr` only. That is the description of
  the change, and it is the author's account of their own work.
- **A delimited generated section in the body** — owned per *section*, not per tool, and
  the one that exists has two writers with different rights. `wtf-code-verify` regenerates
  the verdict, because it ran the probes behind it. `/wtf-create-pr` may only **embed** an
  artifact it did not produce, attributed and aged against HEAD — and where it is
  rewriting a body that already holds a `verify:` section and it has no artifact of its
  own, that section is a **survivor**: carry it across verbatim except for its provenance
  line, which is re-aged against HEAD (`Verified at` versus `git rev-parse --short HEAD`)
  so a carried verdict cannot claim to be current. Only the tool that produced a section
  may drop it. Neither writer may put the other's kind of content in it.

This is why a verification section goes in the **body** rather than only in a comment: a
reviewer opening the PR reads the description, while a comment competes with every thread
and bot on it and scrolls away from the code it is about. The markers are what make that
write safe to repeat.

A tool that finds the *description* stale therefore **reports the drift and names
`/wtf-create-pr`**, whose update path already carries the survivors across; it does not
rewrite the description or the title itself. This holds for tools added later as well as
the three named above: a new tool may claim a delimited section, stating who else writes
it and with what rights, and claims nothing else.

## Scrub the text, and name a key rather than its value

The output these tools quote is deliberately unfiltered where it was captured: a probe's raw
stdout, a failing request, a stack trace. That is the right call at capture time — filtering
there is what launders a compile failure into "the expected failure" — which is exactly why
the scrub belongs here, at the last step before the text is public.

So assume the raw bytes carry something that cannot be published, because routinely they do:
an `Authorization: Bearer …`, a `postgres://user:password@host`, an API key echoed by a
verbose client, a DSN printed by a failed connect. The same applies to identity that belongs
to a work account rather than to the code — a work email address, an internal hostname, an
internal ticket URL — which is a leak of the same class on a public repo even though nothing
about it looks like a secret.

Quote only the lines that carry the assertion, replace a credential with
`<redacted: SESSION_TOKEN>`, and leave the unabridged capture in the scratch directory where
it belongs. Naming the key rather than its value keeps the report readable — a reader learns
which credential was in play, which is the part that mattered — without publishing the one
thing that must not go up.

## Look at every image before it goes up

A text scrub has a handle: the key names are known, so it can be checked mechanically. Pixels
have no such handle. A screenshot carries whatever was on the screen — a notification toast, a
browser tab title, an avatar, a token in the URL bar, an autocomplete dropdown, a window
behind the one you meant to capture — and no rule catches those, because they are not in a
field anyone can grep.

The guard is therefore procedural rather than mechanical: open each image at full size and
look at it before attaching it. If it cannot be looked at, it does not go up. This is also
why a screenshot is the wrong container for text — a JSON response, a terminal transcript, a
log. Paste those as text, which is searchable, copyable, diffable, legible to a screen
reader, smaller, and scrubbable by the section above.

## Delimit a generated section so a rerun replaces it

Any section a tool generates into a PR body will be generated again — by the next run, after
the next commit, or after the verdict changes. Without delimiters the second run appends, and
a body that holds two verdicts oldest-first is how a stale "Verified" outlives the run that
retracted it. The reader takes the first one they meet as current.

**The same verdict is stranded by a tool that changes the code and writes no section.** The
merge below only replaces a section on the next run that writes one, so a fix applied after
a verification was published leaves it attesting to a tree that no longer exists — and a
reader takes it as current for the same reason. That is this failure reached from the other
direction, and no delimiter prevents it.

A tool that edits code where a published section may exist therefore reads the body for the
markers and says the verdict is stale, naming what would refresh it. **The tools this binds
today are `/wtf-code-review`, `/wtf-code-review-no-lenses`, `wtf-deslop` and
`wtf-code-verify`** — named rather than left as a class, because a class with no membership
list gets wired into whichever members someone remembers. Add a tool to that list when it
starts editing code.

`wtf-code-verify` is on it for the case that looks like an exemption and is not. It writes
the section itself, so a run that posts has already replaced any stale one — but it also
edits code at three steps, and posting is an offer the user can decline. Fix a defect at §7,
decline the offer at §8, and the code has changed with no section written: the condition
above, reached by the one tool that could have refreshed it.

Two things that rule needs to be worth having:

- **A read that fails is not a body with no section.** `gh pr view --json body` comes back
  empty on a rate limit, a 502 or a permission it was never granted, and the marker is then
  absent for a reason that has nothing to do with the PR. Report *could not check for a
  published verification section* rather than silence, which is indistinguishable from a
  clean result — the same distinction the merge below fails loudly to preserve. Leave it
  unbounded: a bound means a `timeout`/`gtimeout` prefix, grants match on the command
  prefix, and `Bash(timeout:*)` licenses `timeout`-prefixed anything — a far wider grant
  than the read it protects. A hung `gh` stalls visibly at the end of a run and can be
  interrupted; a broad grant cannot be taken back.
- **Say it where the stale text is.** The section stays live on the PR while the notice
  lands in a terminal the reviewer reading that verdict never sees. Where the run is
  already posting to the PR, the staleness note goes up with it as its own comment — the
  one channel open to a tool that owns no section.

**The marker name is `verify:start` / `verify:end`, spelled exactly that way.** One literal, named
here, used verbatim by every tool that writes a verification section — `wtf-code-verify` and
`/wtf-create-pr` both. A near-miss spelling is worse than a different section entirely: the
filter below matches the full `<!-- … -->` comment, so a body delimited `foo-verify:start`
survives the filter untouched and the new section lands underneath it. Two verdicts, oldest
first, which is what this whole section exists to prevent.

The merge is `~/.claude/scripts/publish-verify-section.sh`, not a recipe to retype. It used
to live here as a fenced block, which meant the shellcheck glob could not see it and no test
could run it — for code whose failure mode is deleting the author's prose from a public PR.
`Dotfiles/claude/tests/publish-verify-section-smoke.sh` now covers every branch below.

```bash
# Fail loudly on a failed read. The redirect truncates body.raw before gh writes a byte, so
# an empty file is indistinguishable from a PR with an empty body.
gh pr view <n> --json body -q .body > "$OUT/body.raw" \
  || { echo "could not read the PR body — nothing written" >&2; exit 1; }

# Replaces the section in place, or appends it when the body has none. Refuses on a body
# whose markers are doubled, out of order or unterminated, on an empty section, and on any
# merge that would change a byte outside the section.
~/.claude/scripts/publish-verify-section.sh merge \
  "$OUT/body.raw" "$OUT/SECTION.md" "$OUT/body.md" || exit 1

# Re-read immediately before the write and refuse on a change. Everything above is a
# read-modify-write over a body other people and bots also edit, and `--body-file`
# replaces the whole thing -- so a review bot that posts between the read and the
# write has its edit silently discarded, with nothing in the output to say so.
#
# This narrows that window; it does not close it. `gh pr edit` exposes no ETag or
# version precondition, so there is no compare-and-swap to be had here and an edit
# landing between this check and the write is still lost. Treat it as best effort:
# where this check keeps tripping, the body is being edited faster than a
# read-modify-write can land, so post a comment instead rather than forcing it.
gh pr view <n> --json body -q .body > "$OUT/body.now" \
  || { echo "could not re-read the body; not writing" >&2; exit 1; }
cmp -s "$OUT/body.raw" "$OUT/body.now" \
  || { echo "the body changed while this ran; re-read and redo rather than overwrite" >&2; exit 1; }

gh pr edit <n> --body-file "$OUT/body.md"
```

Every guard fails in the same direction if you drop it: the body that gets pushed is missing
something the author wrote, or claims something no run produced. Three are worth naming
because each answers a case that looks like success:

- **A marker inside a fenced code block is not a live section.** A body that quotes this
  mechanism — plausible on any PR about it — carries the literal markers, and a filter
  matching on substring alone deletes the quoted lines and everything between them at
  exit 0. The script tracks fence state and leaves them alone.
- **A section is replaced where it sits.** Deleting it and appending at EOF loses no bytes
  and still moves the section below whatever followed it, on every rerun — which is why
  the ownership rule's "every byte outside the markers is carried across untouched" is not
  the whole invariant.
- **So the gate is the whole invariant, checked rather than asserted.** The body with its
  live section removed must be byte-identical, and in the same order, before and after. That
  one comparison catches both cases above and anything else that reaches outside the
  markers, and the script refuses the write rather than reporting it afterwards.

## Put what cannot be regenerated in a comment

A body gets rewritten; a comment does not. That asymmetry decides where a thing belongs.

The body holds the description of the change, because that is what a later run can reproduce
from the diff — and, by the ownership rule above, a generated section a rerun regenerates
the same way. Anything that cannot be reproduced belongs in a comment — an attachment, a
stack trace someone pasted, a reviewer's added note, a decision recorded in prose. A comment
is timestamped and attributable, it survives every rewrite of the body, and superseding it
later is a second comment rather than a silent overwrite of someone else's text.

The test is regenerability, not who is reading. A verification section goes in the body
because the next run rebuilds it from its own probes; the raw capture it quotes stays in
scratch, and a human's paste of that capture stays in a comment.

Two consequences worth stating, because they are where this gets applied wrong:

- **A create is not a rewrite.** `gh pr create --attach` puts the attachment in the body it
  is authoring, and there is nothing there to clobber. The rule bites on the *next* rewrite:
  anything attached or pasted after the create goes in a comment.
- **A rewrite carries the survivors across verbatim.** Attachments, checklists, template
  sections, `Closes #123`, and anyone else's prose are not descriptions of the change and are
  not regenerable from it. A regenerated body that drops the issue link silently stops it
  closing on merge, and one that drops an image leaves an orphaned upload and a broken
  reference. **A live `verify:` section is on this list too** for the rewriting tool, per
  **Who may write what** above: regenerable is not the same as regenerable *by you*, and only
  the tool that produced a section may drop it.
