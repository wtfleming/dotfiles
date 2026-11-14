# Lens: dependencies

**Looks for:** new dependencies (necessity, maintenance, transitive weight), a
version bump whose release notes between the two versions name a break the diff
does not handle, breaking changes to public interfaces, config formats or CLI
flags, irreversible migrations, a new required environment variable, secret or
binding the code reads with no default that nothing in the diff provisions at
runtime — the deploy manifest, CI config or secret store, since an
`.env.example` entry documents a variable without supplying it anywhere a deploy
will look.

## Version bumps

Read the release notes or changelog between the old and new versions —
`gh release view` for a GitHub-hosted package, otherwise the changelog in the
package's own source where the tree has it — for breaks the diff does not show.
You have no web tools, so say when neither was available. A finding cites the
note it rests on.

## Not applicable

A change that adds no import, alters nothing a caller depends on — an exported
signature, a config key, a CLI command name or flag — and adds no migration has
nothing for this lens. A change can touch no manifest at all and still break a
public contract, so decide from the diff, not from the file list.
