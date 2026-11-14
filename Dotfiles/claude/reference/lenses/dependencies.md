# Lens: dependencies

**Looks for:** new dependencies (necessity, maintenance, transitive weight), a
version bump whose release notes between the two versions name a break the diff
does not handle, a new required environment variable, secret or
binding the code reads with no default that nothing in the diff provisions at
runtime — the deploy manifest, CI config or secret store, since an
`.env.example` entry documents a variable without supplying it anywhere a deploy
will look.

A break this change makes for its own callers — a migration, a wire format, an
exported signature, a config key or CLI flag — belongs to the `compatibility`
lens.

## Version bumps

Read the release notes or changelog between the old and new versions —
`gh release view` for a GitHub-hosted package, otherwise the changelog in the
package's own source where the tree has it — for breaks the diff does not show.
You have no web tools, so say when neither was available. A finding cites the
note it rests on.

## Not applicable

A change that adds no import, bumps no version and reads no new environment
variable or secret has nothing for this lens. A change can touch no manifest at
all and still read a variable nothing provisions, so decide from the diff, not
from the file list.
