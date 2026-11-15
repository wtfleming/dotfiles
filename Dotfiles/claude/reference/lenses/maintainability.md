# Lens: maintainability

**Looks for:**

- a name that says something the code does not do, functions doing several
  things, unactionable error messages, changes bundling unrelated concerns
- code the change adds that could be deleted outright: handling for a case that
  cannot occur, an option no caller passes a second value to, a wrapper that
  only forwards, a fallback for a failure the platform already rules out
- reuse: logic the repo already implements elsewhere, a second copy of
  something within the diff itself, a hand-rolled version of what a dependency
  already in the manifest provides, new UI markup that re-implements a component
  the project's shared component library already provides, a new abstraction
  where an existing one would have served, code shared between two things that
  only look alike
- code the change orphaned but did not remove: a function whose last caller
  went away, a config key nothing reads, a flag now permanently on with its dead
  branch intact

## Evidence bar for reuse, orphans and unneeded code

Both the duplicate and the orphan live in files the change did not touch, so
each such finding is an assertion about code nobody else in this run reads.
Search before asserting an absence, and count re-exports, string-keyed lookups
and dynamic dispatch as callers. "Something like this probably already exists"
and "nothing uses this any more" are not reportable without the search behind
them. The orphan claim is the more dangerous, because a wrong one invites a
deletion.

A claim that added code guards against nothing invites a deletion too. Name why
the state cannot occur and where that is proven: the type end to end, the deploy
topology, every call site, the flag's value in every environment. A pattern name
alone is not a finding. Three things are never unneeded, whatever the internal
types say:

- a check on data that crossed a trust boundary — a request, a file, the
  environment, deserialised input. A static type does not survive that crossing.
- a deliberate second security layer
- a single-implementation interface the tests substitute

## Anchors

A duplication finding anchors at the changed code and cites the existing
implementation by `file:line` in the finding itself. Anchoring at the
pre-existing copy would collapse two added duplicates into one finding when the
reports are merged. An orphan finding anchors at the orphaned code, since that
is the line that gets deleted.

## Duplication worth leaving alone

Judge by whether the two copies have to change together, not by how alike they
look.

## Not applicable

Only when the scope holds no code this lens could govern at all.

The evidence bar for unneeded code is adapted from GSD Core's necessity review,
itself from l-gevity-skills (both MIT, see `~/.claude/reference/LICENSE-gsd`).
