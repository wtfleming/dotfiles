# Reading production, whatever it is plumbed into

A production-impact assessment needs facts about a live system. Which vendor holds those
facts is an accident of the repo you are standing in: Datadog here, Sentry and Grafana
there, CloudWatch and nothing else somewhere else. An assessment written against one
vendor's tool names stops working the moment it meets another.

So this reference is organised around **the four questions**, not around vendors. For each
question: who can answer it, what a thin answer looks like, and — the part that matters
most — what follows when nothing can answer it at all.

## Contents

- The read-only rule
- Discovering what is reachable
- Q1. What is actually deployed right now?
- Q2. How much does the changed path actually get used?
- Q3. What is already failing?
- Q4. Is the change gated?
- When a question cannot be answered

## The read-only rule

You are reading production. Every call you make must be a read.

Most telemetry catalogues ship writes next to the reads — dashboards get upserted,
feature flags get toggled, rollouts get updated, annotation queues get appended to. Those
are in reach of the same connection you are using to read, and a single one of them makes
this agent a thing that *changes* production rather than a thing that describes it.

**The verb test.** If the tool's name or its description says it creates, updates,
deletes, removes, toggles, upserts, sets, adds, registers, posts, promotes, archives or
applies — do not call it. Not to check, not to test the connection, not with arguments
you believe are inert, not once. A read that is refused costs a line in the report; a
write costs an incident of its own.

**Describe before you invoke.** For any tool you did not get from the reference below,
read its schema first and decide from that. A name alone is not enough — `get-*` naming
is a convention, not a guarantee.

**This rule is prose, and prose is all there is.** A tool allowlist cannot express
"read-only" across vendors nobody has enumerated yet, which is the same breadth that lets
this work at all. The compensating control is disclosure: the report names every tool you
called, so a write is at least visible afterwards. Keeping that list honest is part of the
job, not bookkeeping.

## Discovering what is reachable

Two steps, in this order, and do not skip the first.

**Read the repo for who it reports to.** The manifests say which vendors this service
actually uses, which is a smaller and more accurate set than whatever your connection
happens to expose. Grep the tree for the config that names one:

Two searches, because the signals come in two kinds. Vendor names appear in file
*contents*; platform manifests announce themselves by *file name* and do not contain their
own name — a `fly.toml` holds `app` and `primary_region`, never the string `fly.toml`.

```bash
# Contents. `git grep` searches tracked files only, so .git, node_modules, vendor/ and
# anything gitignored are all skipped -- which is both far faster on a large repo and
# more accurate, since a vendored transitive dependency or an old commit object names a
# vendor this service does not report to.
#
# printf in a loop, not `sed "s|...|"`: every pattern below contains `|`, which would
# collide with the delimiter and error on every iteration -- printing nothing, which reads
# as "this repo reports to no vendor" and is the exact false green this method exists to
# avoid. `awk -v` has its own quoting layer and would print split.io for split\.io.
for v in 'datadoghq|DD_(SERVICE|ENV)' 'newrelic|NEW_RELIC' 'sentry|SENTRY_DSN' \
         'honeycomb|OTEL_EXPORTER|opentelemetry' 'prometheus|grafana' 'splunk|loki' \
         'argocd|argoproj' 'launchdarkly|LD_SDK' 'statsig|unleash|flagsmith|split\.io'; do
  git grep -lE "$v" | head -3 | while IFS= read -r f; do printf '%s -> %s\n' "$v" "$f"; done
done

# File names. `git ls-files` rather than `find`, for the same reason as above: `find
# -maxdepth 3` still descends node_modules, and ./node_modules/pkg/vercel.json matches at
# exactly depth 3.
manifests='fly\.toml|vercel\.json|wrangler\.toml|serverless\.ya?ml'
manifests="$manifests|[Kk]ustomization\.ya?ml"
git ls-files | grep -E "(^|/)($manifests)\$"
```

**Cap the output per vendor, never across all of them.** One `head` over a combined
pattern truncates by traversal order, so in a monorepo where twenty files mention one
vendor a second vendor's only config is dropped and you conclude the repo does not report
to it — which sends a question unanswered for no reason. The loop above caps each pattern
separately for exactly that reason, which is also why it is a loop rather than one
combined pattern.

**Match on a key's name; never read its value.** `SENTRY_DSN` and `DD_API_KEY` are
credentials, and `git grep -l` prints file names only, which is the point. What you need
from these files is the vendor and the service name — never the secret, which has no
bearing on any of the four questions and should not be pulled into a transcript. A Sentry
DSN is itself an event-submission credential.

Also worth opening: `.github/workflows/` for what deploys and how, a `Dockerfile` or chart
for the service name it runs under, and any `terraform/` for the resources it owns.

**Then confirm against the tool surface.** The repo naming a vendor does not mean you can
query it here — the tools may not be connected in this session, or the credentials may
not be. Look for read tools belonging to the vendor the repo named, however this session
exposes tools: sometimes as a vendor's own server, sometimes through something that fronts
several of them and has to be searched by vendor name first. Discover rather than guess —
a remembered tool name is not evidence a tool exists, and names differ between one
environment and the next even when the vendor is the same.

A vendor the repo uses and this session cannot reach is simply unreachable, no matter what
the repo says. That is a question left unanswered, not an error, and the last section says
what follows from it.

A repo may name a vendor the tools cannot reach, and a connection may expose a vendor the
repo does not use. **Only the intersection is evidence.** Querying a vendor this service
does not report to returns somebody else's numbers, which is worse than no numbers.

## Q1. What is actually deployed right now?

Everything else depends on this. Telemetry describes the code that is running, and if that
is not the base of your branch then the numbers describe different code. A branch that
forked three weeks ago has a merge base that predates two releases.

| Answers it | What to ask for |
|---|---|
| GitOps controllers (ArgoCD, Flux) | the application's synced revision and health |
| Kubernetes | the image tag actually running, and replica counts mid-rollout |
| Platform hosts (Vercel, Fly, Heroku, Cloud Run) | the current release and its commit |
| Error trackers (Sentry and most others) | the latest **release**, which is usually tagged with a commit |
| CI/CD (GitHub deployments, pipeline runs) | the last successful deploy to the production environment |
| Deploy events in a metrics vendor | change-tracking or deployment events, newest first |

**Cheapest fallback when none of them answer:** `git log` the default branch and take its
tip, and then *say in the report that you assumed it*. **It is an assumption, and it never
counts as an answered question for the verdict rule** — it reads the repo, not production,
so a session that reached no provider is `not assessed` even though this came back with
something. The assumption is usually right and
occasionally very wrong — a release train, a frozen branch, a failed deploy nobody
retried — and the reader can check it in seconds where you cannot.

Record the revision you settled on. Every later reading is "as of" that revision.

## Q2. How much does the changed path actually get used?

This is what turns a finding from a guess into a size. A lock on a table taking 40 writes
a second is an outage; the same lock on a table taking 40 writes a week is a maintenance
window nobody notices.

| Answers it | What it gives you |
|---|---|
| Metrics vendors | request rate, error rate, latency percentiles, queue depth, connection counts |
| APM / tracing | per-endpoint and per-query throughput, the actual callers of a span |
| Error trackers | transaction volume and user counts, but only for instrumented paths |
| Logs | a countable line, when nothing else is instrumented — count the log template |
| Load balancer or CDN metrics | request volume when the app itself is not instrumented |

**Ask for a window wide enough to show shape, not just level.** An hour tells you the
current number; thirty days at daily rollup tells you whether it is climbing. A long
window and a look at the trend gets most of the way, and anomaly functions in the vendor's
own query language get the rest. Do not reach for a forecasting model.

**Prefer the narrowest series that still answers the question.** Service-wide request rate
does not tell you about the one endpoint the diff touched, and a trajectory built on it
will be refuted by the first person who reads it.

## Q3. What is already failing?

Two uses. A change landing on a path that is already erroring has a different risk profile
from one landing on a quiet path. And a failure mode you are about to introduce may
already be happening, which converts a hypothesis into an observation.

| Answers it | What it gives you |
|---|---|
| Error trackers (Sentry, Rollbar, Bugsnag, and the error products inside metrics vendors) | grouped issues, counts, first-seen, affected releases and users |
| Logs | error templates and their counts, when no tracker is wired up |
| Monitors and alerts | what the team already considered worth waking up for, and what is firing now |
| SLOs | remaining error budget, which is the most decision-relevant number available |

A monitor's existence is itself evidence: it tells you which failure modes this team has
already been burned by.

## Q4. Is the change gated?

A change behind a flag that is off in production cannot degrade production today, and that
single fact refutes more trajectories than any metric read. The inverse matters too — a
flag already at 100% is not a safety net, and removing a flag whose default differs from
its current served value is a behaviour change disguised as cleanup.

| Answers it | What to ask for |
|---|---|
| Flag platforms (LaunchDarkly, Statsig, Unleash, Flagsmith, Split) | the flag's state **in the production environment**, its rollout percentage, and its default |
| The repo | flag names the diff reads, and any hardcoded default |
| Config and env | a kill switch that is not a flag platform at all |

Ask for production specifically. A flag on in staging and off in production is the normal
case, and reading the wrong environment inverts the answer.

## When a question cannot be answered

This is the section that keeps the whole method honest.

An unanswered question is **not** a passing answer. No error-rate series for an endpoint
can mean the endpoint is healthy or that nothing instruments it, and those two have
opposite consequences. Treat them as opposite:

- **A reading that contradicts the chain** refutes it. Say which reading, with its query
  and window.
- **No reading available** leaves the chain unsettled. It stays in the report as unsettled,
  and it names which of the four questions went unanswered and why — vendor not reachable,
  path not instrumented, or service not mapped.

Where *every* question went unanswered, there is no assessment to give. Say that, plainly,
and do not convert it into a clean bill of health. A reader who learns that production
could not be read is in a good position; a reader handed silence believes the opposite of
the truth.
