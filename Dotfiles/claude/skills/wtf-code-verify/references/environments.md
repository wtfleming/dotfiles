# Environments by tier

How to run a probe in a project you do not have memorized, and the hazards that come
with each tier and each language. Pick the tier in SKILL.md §5 first.

## Contents

- Find the project's own commands before inventing any
- What CI already runs
- Pinned toolchains (mise, asdf)
- Per-language quick reference
- Tier 0 — a test in one package
- Tier 1 — headless script, CLI, or a real build
- Containers, compose and other isolation
- Tier 2 — the service booted, driven over its real interface
- Tier 3 — full stack and a browser

When a claim uses a differential, the invocation must be **identical on both sides**.
The baseline worktree and the main checkout differ by one commit; if they also differ
by how you ran the probe, you are comparing two procedures rather than two commits.

## Find the project's own commands before inventing any

Most repositories already encode how to run themselves, and a hand-rolled invocation
is a good way to reproduce a setup gap instead of a bug. In rough order of authority:

```bash
cat CLAUDE.md AGENTS.md README.md 2>/dev/null | head -100
ls .claude/commands .claude/skills 2>/dev/null       # project-specific workflows
cat Makefile justfile Taskfile.yml 2>/dev/null | head -40
jq -r '.scripts | keys[]' package.json 2>/dev/null   # node
ls mix.exs rebar.config Cargo.toml Eldev Cask 2>/dev/null
ls docker-compose*.y*ml compose*.y*ml 2>/dev/null
```

Prefer a project's own `dev`/`start`/`test` entry point over composing one yourself.
It usually encodes ordering and health gates that are invisible from the outside and
painful to rediscover.

`baseline-worktree.sh detect` prints which ecosystems it found, and a repo can
legitimately be several at once.

## What CI already runs

Check before designing probes, not after. A probe that duplicates a job running on every
push has spent minutes to reproduce a green check.

```bash
ls .github/workflows .gitlab-ci.yml .circleci 2>/dev/null
grep -rhE '^\s{4,}(run|- run):' .github/workflows 2>/dev/null | sort -u | head -30
```

Read what the jobs actually invoke, and note where they stop. Most pipelines run the unit
suite and the linter, some run an integration suite against a service container, and very
few drive a browser or exercise an authorisation boundary between two real users. The
probes worth building are the ones on the far side of where CI stops.

Two things this discovery gives you beyond scope. The workflow file is often the most
accurate documentation of how to run the project — it is executable, so it cannot drift
the way a README does. And a job that boots services shows you the exact environment
variables, service versions and health gates the project needs, which is otherwise the
most tedious thing to reconstruct.

Say what you found in the report, so the run's contribution is legible rather than
implied.

## Pinned toolchains (mise, asdf)

A project that ships `.tool-versions`, `mise.toml` or `.mise.toml` pins its compiler
and runtime per directory. Two consequences matter here.

The pin is a **tracked file**, so the baseline worktree inherits it and both sides run
the same toolchain — which is what you want, and one fewer variable.

But the manager only takes effect if something invokes it. A non-interactive shell
has usually not run mise's shell hook, so `mix`, `erl` and friends are simply absent
from PATH. The resulting "command not found" in the baseline looks exactly like a
broken tree. `baseline-worktree.sh detect` reports the pin and whether a manager is
installed; when one is, it runs every bootstrap step through `mise exec --` and tells
you to use the same prefix. **Use it on both sides**, or you have compared two
toolchains rather than two commits:

```bash
mise exec -- mix test test/foo_test.exs:42
```

One case where the toolchains legitimately differ: a change that bumps the pin. Then
the two trees run on different versions by design, and that is part of what you are
verifying rather than a flaw in the setup. The script says so when it sees the pin in
the diff.

## Per-language quick reference

| | Scope a test to one thing | Gitignored build state | The stale-artifact hazard |
| --- | --- | --- | --- |
| **Node** | `<pm> test -- <pattern>`, or the workspace filter flag | `node_modules/`, `dist/`, generated code | Package B type-checks against A's *built* output. If A changed and was not rebuilt, B reports "has no exported member" for a symbol that plainly exists. |
| **Rust** | `cargo test <name>` (prints "N filtered out"), `-p <crate>` in a workspace | `target/` | Cargo tracks staleness well. The cost is time: a fresh worktree has no `target/` and recompiles everything. `cargo fetch` then `cargo build --offline` keeps it off the network. |
| **Elixir** | `mix test test/foo_test.exs:42`, `mix test --failed` | `_build/`, `deps/` | `MIX_ENV` splits the build tree. Compiling under `dev` does nothing for a `test` run, and a `_build` left over from another branch can serve stale beam files. |
| **Erlang** | `rebar3 eunit --module=foo`, `rebar3 ct --suite=...` | `_build/` | Same shape as Elixir: profile-scoped `_build` trees, and old beams shadow recompiled sources. |
| **Elisp** | `emacs -Q --batch -L . -l ert -l foo-test.el -f ert-run-tests-batch-and-exit` | `*.elc` | A stale `.elc` silently shadows the `.el` beside it, even when the source is newer. A worktree has none and your main checkout may, so the two sides load different code. |

Three Elisp specifics worth knowing, because they decide whether you tested the tree
at all:

- `--batch` already skips your init file (`user-init-file` is nil), so your personal
  config is not the variable. `-Q` additionally skips site-lisp; harmless to include.
- `-L .` is what points `require` at the tree under test. Without it you may load an
  installed package from ELPA and test neither side.
- ERT writes its results to **stderr**, not stdout, and exits 1 on failure. A probe
  that captures only stdout looks like it produced nothing at all.

Exit codes are not uniform across runners, so read the output rather than trusting a
single convention: ExUnit exits **2** on a failed test, ERT exits 1, cargo exits 101.
`mix test` also compiles under `MIX_ENV=test` on its own, so a dev-profile compile in
the bootstrap neither helps nor hinders it.

## Tier 0 — a test in one package

Run the project's test command scoped as narrowly as it allows: a single file, a
single name pattern, a single crate or module. A full suite run buries the one result
you care about and takes long enough that you will be tempted to skip the baseline
half.

Two things to check before believing a failure. **Stale artifacts** — see the table
above; each ecosystem has its own version, and every one of them produces errors that
read like defects. And **cache replays**: an instantaneous green from a caching task
runner is a replay, not a run. Force a real execution before citing it, and note that
a replayed log may print paths from wherever the cache entry was originally produced,
which looks alarming and means nothing.

Prefer this tier. A probe that lives here is also the best candidate for promotion to
a permanent test — see `promotion.md`.

## Tier 1 — headless script, CLI, or a real build

The best probe at this tier is usually one the project already has: a generator, a
renderer, a compiler, a CLI subcommand that takes real input and writes real output.
Run it into two directories and diff them.

```bash
diff -r baseline-out/ head-out/
```

That diff is the natural probe for a refactor and for anything that changes emitted
output, and it needs no assertions written in advance — the bytes are the assertion.

When you need to call an API directly, write a throwaway script in the scratch
directory rather than in the repo, and keep it byte-identical between runs. Run it
with the worktree as cwd, or pass the tree as an argument; editing the script between
runs makes it the variable. Language-appropriate one-shots:

```bash
cargo run --example <name>                        # rust
mix run -e '<expr>'                               # elixir
erl -noshell -eval '<expr>' -s init stop          # erlang
emacs -Q --batch -L . -l <lib> --eval '<expr>'    # elisp
```

**Watch for live inputs.** A build that fetches from a database or an API is not
reproducible on its own — content that changes between the two runs shows up in the
diff looking exactly like a code difference. Run both sides close together, and if a
diff looks like content rather than structure, re-run the baseline to see whether it
is stable.

## Containers, compose and other isolation

Isolation is not a virtue in itself. It buys attributability — a failure belongs to the
code rather than to your checkout — and containment, so the probe leaves nothing behind.
Buy only what the claim needs.

**Use what the project already has.** In order: its compose file
(`docker-compose*.y*ml`, `compose*.y*ml`), its devcontainer, its Makefile or justfile
target, its documented `dev` script. These encode service versions, health gates and
ordering that are invisible from the outside and painful to rediscover.

```bash
docker compose up -d db redis        # dependencies only, app from the tree under test
docker compose ps                    # confirm health, not just "started"
```

Bringing up **only the dependencies** and running the application from the worktree is
usually the right shape. The app is what you are testing, so you want it built from the
tree under test with a debugger and logs to hand; the database and cache are stage
scenery, and they can stay up across both sides of a differential because they are state
rather than code.

**Do not build an image the project does not have.** A hand-rolled Dockerfile turns the
session into Dockerfile debugging, which teaches you nothing about the change and
produces a verdict about your image. If the project has no container story and the claim
needs one, drop a tier and say in the report which part went unexercised as a result.

**Containers are not a substitute for a fresh entity per run.** A container shared across
two runs carries its volume with it, so the baseline's rows are still there for the head
run. Reset the volume between runs (`docker compose down -v`) or use distinct entities —
and if you reset, remember it also discards any seed the project's setup put there.

**Tear down what you started**, and say so in the report's residue line. A leftover
container holding port 5432 is the next session's inexplicable connection error.

## Tier 2 — the service booted, driven over its real interface

Use the project's own startup path, and its health gate if it has one. "The process
started" and "the service is ready" are different claims, and probing between them
produces a connection error that reads like a bug.

**The two runs cannot overlap.** Both trees want the same ports. Run one side,
capture, stop, then the other. Shared dependencies in containers (a database, a
cache, a queue) are state rather than code, so leave them up across both runs.

**The database is the hazard at this tier.** Both runs read and write the same rows.
If the baseline run creates the record that makes the HEAD run pass, the differential
measured your test setup. Pick one: a fresh entity per run, a reset between runs, or
run the pair in both orders and confirm the verdict does not move. Some ecosystems
solve this for you inside the test suite — Ecto's SQL sandbox rolls each test back —
but that isolation does not extend to a probe driving the service from outside.

Capture whole responses, including status and headers:

```bash
curl -s -i http://localhost:<port>/<path>
```

A probe that greps one field out of a 500 will report "field missing" as though it
were the bug.

**On the BEAM**, a long-lived node keeps state that outlives a request — supervisors,
ETS tables, GenServer state, cached config read at boot. Restart the node between the
two runs rather than reloading code into a running one, or you are measuring a hybrid
of both trees.

## Tier 3 — full stack and a browser

Reach here only when the symptom needs a browser to execute the output — hydration,
interaction, layout, anything visual.

**Prepare apps through the task runner, not a package's own build script.** Running
the script directly skips the dependency graph, so upstream artifacts and generated
code never get produced and the app fails with a wall of resolution errors that look
nothing like what they are.

**Check for a held browser profile before driving one.** Browser-automation servers
typically hold a single shared profile directory, and a second attach fails quietly —
returning no pages, which reads exactly like a passing check. If the profile is
locked, close the stale browser, use a different automation server, or drop to an SSR
probe: fetch the page with `curl` and assert on the served HTML. That proves server
output without a browser, and it is a real result as long as the report says the
client-side behaviour went unexercised.

Screenshot both sides at the same viewport and the same path, and save both. A single
screenshot of the fixed state is a picture, not evidence.
