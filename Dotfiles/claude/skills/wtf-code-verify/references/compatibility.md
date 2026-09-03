# Deploying alongside the old version

Every probe elsewhere in this skill assumes one version of the world. Production does not
have one. Between the first process taking the new code and the last one losing the old,
both are live against a single database. A job enqueued a minute ago is consumed by
whichever version picks it up. A client three releases old is still calling. And rollback
puts the old code back on top of data the new code wrote.

The method is the differential you already have — two trees, one probe — asked a
different question. Not "did the behaviour change" but "do these two versions survive
each other".

## Contents

- When to run this at all
- The deploy window: two probes
- Migrations
- Consumers
- In-flight work
- Rollback

## When to run this at all

Most changes need none of it. Skip when the diff touches no schema, no persisted format,
no message payload, no public interface, and nothing another deployable reads.

```bash
git diff --name-only <base>...HEAD \
  | grep -iE 'migrat|schema|\.sql|proto|openapi|graphql|\.avsc|serializ|version'
```

The file list is a hint, not the test. Also in scope: a new required column or field, a
renamed or removed one, a changed enum, a changed default, a new queue or topic, a
changed message shape, a bumped API version, anything exported from a package other code
imports.

If the project deploys atomically — one process, a CLI, a library released as a version
people opt into — the deploy window does not exist, and only **Consumers** and
**Rollback** apply.

## The deploy window: two probes

During a rolling deploy both of these are true at once. Each is its own probe.

**New code against the old schema.** The new version boots before the migration
completes, or the migration is deliberately run afterwards, which is the safer order for
a lock-heavy one. Run HEAD against a database migrated only as far as the base.

**Old code against the new schema.** The migration lands first and old processes are
still serving. Migrate the database to HEAD, then run the **baseline** worktree against
it. This is the probe that catches `SELECT *` mapped onto a struct, a `NOT NULL` column
the old insert does not supply, and a dropped column the old reader still names.

```bash
BASE=$(~/.claude/skills/wtf-code-verify/scripts/baseline-worktree.sh path baseline)
# ... migrate the database to HEAD ...
(cd "$BASE" && <the old version's probe>)
```

**Expected result for both is pass**, and that is worth saying out loud before you run
them. Every other section of this skill trains the reflex that a green baseline means a
broken probe. Here a green baseline is the answer you want, and the failure is the
interesting result.

The deploy order a change *requires* is itself a finding. If the new code cannot run
against the old schema, the change is not deployable in one step and needs splitting —
add the column nullable, deploy, backfill, deploy the code that depends on it, then
tighten the constraint. Say so. That is a design finding worth more than any probe
result, and it is invisible from a diff.

## Migrations

Four checks, and usually only the first has ever been run.

**Forward, on realistic data.** An empty table proves the SQL parses. Volume proves what
it does to production: `ADD COLUMN NOT NULL DEFAULT`, an index built without
`CONCURRENTLY`, a type change that rewrites the table. Restore or generate a realistic row
count, time it, and note what it locks and for how long.

**Backward, actually executed.** Down migrations are written once and run never, which is
how you discover at 2am that yours drops a column that was renamed rather than added. Run
it, then run the forward migration again; the pair should round-trip.

**Re-runnability.** A migration or backfill interrupted halfway gets started again. Does
the second run succeed, or fail on a constraint the first run created?

**Rows written during the backfill.** A backfill reads a snapshot while writes keep
arriving. Does the new code write the new shape from the moment it deploys, or is there a
window whose rows nothing ever backfills?

## Consumers

"Who else reads this?" — and the answer is rarely just the application in front of you.

- other services calling the API: run their contract tests, or their client, against the
  new server
- clients you do not control — a mobile app three releases behind, a partner integration,
  a script someone wrote once
- rows already stored in the old format, read by the new code
- cached values written by the old version and read by the new one, especially anything
  with a long TTL
- serialized state: sessions, signed tokens, flag payloads, anything persisted as a blob

The probe has one shape throughout: exercise the **new** code with input produced by the
**old** one. Generate that input in the baseline worktree rather than hand-writing what
you believe the old format looked like — hand-written old data encodes your belief about
the format, and if that belief were reliable you would not need the probe.

## In-flight work

A job enqueued by the old version is consumed by the new one, and the reverse while the
rollout is uneven. Anything durable and asynchronous is in scope: queues, scheduled jobs,
webhooks in retry, multi-step workflows part-way through.

Enqueue from the baseline worktree, consume on HEAD. Then reverse it. A changed payload
shape that both versions must tolerate is the usual failure, and it surfaces as a
deserialization error in a worker log rather than anywhere a caller can see — so read the
worker's output, not the response.

## Rollback

Rollback is the plan for when everything above turned out to be wrong, which makes it the
one thing that must not itself be a guess.

Run the change forward, produce data with it, then put the baseline worktree back in
front of that data and exercise it. It is the same shape as the deploy-window probe, but
the data now contains whatever the new version wrote — which is the part nobody simulates
and the part that makes a rollback fail.

Where rollback is genuinely impossible — a destructive migration, a one-way format change
— that is not a probe failure, it is a fact about the change. Say it plainly in the
report. A reviewer who learns it from the verification section is in a much better
position than one who learns it during the incident.
