# Lens: compatibility

**Looks for:** a change the old version and the new one cannot survive side by
side. During a rolling deploy both run against one database and one queue, old
clients keep calling, and a rollback puts the old code on top of data the new
code wrote. Hunt:

- **Schema migrations** that are not expand/contract: a column dropped or
  renamed while the deployed code still reads it, a `NOT NULL` column the old
  insert does not supply, a type change, an index built without the database's
  online option, a table rewrite or long lock on a table of real size, a
  backfill that misses rows written while it runs, a migration that cannot be
  reversed or cannot be re-run after failing halfway.
- **Wire and storage formats:** a field removed, renamed, retyped or made
  required in an API response, request, event, job payload or protobuf/Avro
  message; an enum value removed or reordered; a changed default; a stored
  blob, cache entry, session or signed token the new code cannot read in the
  old shape.
- **In-flight work:** a job enqueued by the old version that the new consumer
  rejects, or the reverse while the rollout is uneven.
- **Public interfaces callers depend on:** an exported signature, a config key,
  a CLI command or flag, a route or status code, changed or removed with no
  deprecation path.

## Evidence

Name both sides: what the old version writes or sends, and the line in the new
version that cannot handle it, or the reverse. "Old code against the new
schema" and "new code against the old schema" are separate findings with
separate fixes; say which one breaks. Where the fix is a split into deploy
steps — add nullable, deploy, backfill, tighten — say so; that is the finding.

If the project deploys atomically — a CLI, a library people opt into by
version, a single process — the deploy window does not exist. Report only
what breaks consumers and rollback.

## Boundaries with other passes

- **`dependencies`** owns third-party packages, version bumps, and environment
  variables or secrets nothing provisions. A break *in a dependency* is
  theirs; a break this change makes *for its own callers* is yours.
- **`resilience`** owns a call that fails at runtime. A consumer that crashes
  on an old payload is yours; one that retries it forever is theirs.
- **`performance`** owns a slow query on the happy path. A migration's lock
  duration is yours.

## Not applicable

When the scope changes no schema, no persisted or serialized shape, no
message, and nothing exported, routed or configured that another caller or
deployable reads.
