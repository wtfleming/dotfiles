# Publishing to GitHub

Three tools here write outward: `/wtf-code-review` posts findings, `wtf-code-verify` posts
verification sections, `/wtf-create-pr` composes the pull request itself. What they publish
leaves your control the moment it lands — GitHub indexes it, mirrors it into notification
email, and serves it to anyone who can read the repo. Editing a comment afterwards does not
take back what was already delivered.

This file is the single statement of the guards that apply to all three. Each one points here
rather than restating them, so a fix lands once. Nothing below is about *what* to say; it is
about the four ways an otherwise correct message goes wrong on the way out.

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

Wrap the section in markers and filter the old copy out before appending the new one:

```bash
gh pr view <n> --json body -q .body > "$OUT/body.raw"
# Fail closed on a half-open marker: with a start and no end, the filter below drops
# everything after it, which silently deletes whatever the author wrote underneath.
[ "$(grep -c '<tool>:start' "$OUT/body.raw")" = "$(grep -c '<tool>:end' "$OUT/body.raw")" ] \
  || { echo "unbalanced markers in the PR body — fix it by hand" >&2; exit 1; }
awk '/<!-- <tool>:start -->/{skip=1} !skip; /<!-- <tool>:end -->/{skip=0}' \
  "$OUT/body.raw" > "$OUT/body.md"
{ echo '<!-- <tool>:start -->'; cat "$OUT/SECTION.md"; echo '<!-- <tool>:end -->'; } >> "$OUT/body.md"
gh pr edit <n> --body-file "$OUT/body.md"
```

The balance check is the part worth keeping. A body that lost its end marker to a hand-edit
reads as ordinary Markdown, and the filter that trusts it deletes every line the author wrote
below the section.

## Put what cannot be regenerated in a comment

A body gets rewritten; a comment does not. That asymmetry decides where a thing belongs.

The body holds the description of the change, because that is what a later run can reproduce
from the diff. Anything that cannot be reproduced belongs in a comment — an attachment, a
stack trace someone pasted, a reviewer's added note, a decision recorded in prose. A comment
is timestamped and attributable, it survives every rewrite of the body, and superseding it
later is a second comment rather than a silent overwrite of someone else's text.

Two consequences worth stating, because they are where this gets applied wrong:

- **A create is not a rewrite.** `gh pr create --attach` puts the attachment in the body it
  is authoring, and there is nothing there to clobber. The rule bites on the *next* rewrite:
  anything attached or pasted after the create goes in a comment.
- **A rewrite carries the survivors across verbatim.** Attachments, checklists, template
  sections, `Closes #123`, and anyone else's prose are not descriptions of the change and are
  not regenerable from it. A regenerated body that drops the issue link silently stops it
  closing on merge, and one that drops an image leaves an orphaned upload and a broken
  reference.
