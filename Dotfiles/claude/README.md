Claude Code config
==================

Everything under here is deployed to `~/.claude/` by `sync-dotfiles.sh`, so it
applies in every repo, not just this one. This README is documentation for the
repo and is deliberately not synced.

## Code review

`/wtf-review-changes` reviews recent changes in a **fresh context** — a subagent
that never sees the conversation which wrote the code, so it cannot inherit the
author's assumptions about it.

```
/wtf-review-changes                  # uncommitted, else the branch, else HEAD
/wtf-review-changes HEAD~3           # any ref, branch or path
/wtf-review-changes main --deep      # add a verified parallel pass per dimension
```

Without `--deep` it settles the scope, runs the project's test suite and linter,
reviews the diff against the checklist, and prints findings as Critical, Warning
or Suggestion — then stops. The reviewer has no `Edit` or `Write`, so a review
cannot change anything.

`--deep` adds eight `wtf-lens` agents in parallel, one per lens: correctness,
security, tests, maintainability, performance, dependencies, reuse, resilience. Their reports are
merged and
deduplicated with the reviewer's, then verified before printing: one
`wtf-refuter` per Critical and Warning finding, each told to argue the finding
is *wrong* and to answer refuted when unsure. Suggestions arrive marked
`(unverified)` rather than spending an agent apiece on nits. There is
deliberately no linter lens — the reviewer already runs the real one.

`reuse` and `resilience` are the two lenses with no counterpart in the checklist.

`reuse` is also the only one whose subject sits outside the diff — both the
duplicate it hunts for and the code the change orphaned live in files the change
did not touch — which is why it owns both halves rather than splitting the orphan
case into `maintainability`. One lens, one evidence bar: search before asserting an
absence, cite what you found by `file:line`, and judge duplication by whether the
two copies have to change together rather than by how alike they look.

`resilience` asks what happens when something the code *calls* fails, hangs or
half-succeeds: missing timeouts, retries without backoff, failures swallowed into
a default that reads as success, half-completed work that leaves inconsistent
state.

Both of its neighbours get an explicit boundary in the command, since a lens that
bleeds into the one beside it produces the same finding twice in different words.
`correctness` keeps whether the code computes the right answer from the inputs it
was given. `performance` and `resilience` divide by path — the cost of the happy
path against the behaviour of the failure path.

There is no fix mode. The report lands in the conversation, so to act on it,
say which findings — "fix the first two" — and the fixes happen in the main
session, which knows what you were trying to do. The command never edits and
never commits.

### Design review, earlier in the cycle

`/wtf-design-review` asks a different question at a different time: *is this
change the right shape?* Run it mid-work, while changing course is still cheap —
`/wtf-review-changes` is the pre-PR gate, and design feedback that arrives at
the gate arrives after the sunk cost.

```
/wtf-design-review                   # uncommitted, else the branch, else HEAD
/wtf-design-review src/sync         # any ref, branch or path
```

It dispatches a single `wtf-design-reviewer` agent, cold, with the scope and
nothing else — no summary of intent, no rejected alternatives — so it reviews
the shape rather than the rationale. Output is **Suggestion-only** (design
advice never blocks), and every finding must name a concrete, smaller
alternative and what it buys; "consider making this more modular" is banned by
the agent's own rules. It does not run tests or the linter — mid-work they are
allowed to be red — and it does not hunt bugs.

### The agents

| Agent | Role |
|---|---|
| `wtf-change-reviewer` | scope, tests, lint, the full review |
| `wtf-lens` | one dimension only; dispatched eight times by `--deep` |
| `wtf-refuter` | tries to kill a single finding |
| `wtf-design-reviewer` | shape of the change, Suggestion-only; dispatched by `/wtf-design-review` |

All four are read-only — no `Edit`, no `Write`, and no ability to spawn an agent
that has them. Edits only ever happen in the main session, one approval at a time.

### Tuning it

- `reference/code-review-checklist.md` sets the priority order. The reviewer
  reads the deployed copy at review time, so editing it changes behaviour
  without touching an agent definition.
- A project's own `REVIEW.md`, `AGENTS.md` or `CLAUDE.md` wins where it conflicts.
  `REVIEW.md` is the name Anthropic's own code review reads.
- The eight `--deep` rubrics live in the command, not in `wtf-lens`, so they can be
  retuned without editing an agent.

### Cost

`--deep` spawns one reviewer, eight lenses, and one refuter per verified finding —
tens of agents on a real branch. It announces each fan-out before spawning it, so
the spend can be refused. For very large diffs, the built-in `/code-review ultra`
is the maintained alternative.
