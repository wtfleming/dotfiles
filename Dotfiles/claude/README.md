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

`--deep` adds six `wtf-lens` agents in parallel, one per dimension: correctness,
security, tests, maintainability, performance, dependencies. Their reports are merged and
deduplicated with the reviewer's, then verified before printing: one
`wtf-refuter` per Critical and Warning finding, each told to argue the finding
is *wrong* and to answer refuted when unsure. Suggestions arrive marked
`(unverified)` rather than spending an agent apiece on nits. There is
deliberately no linter lens — the reviewer already runs the real one.

There is no fix mode. The report lands in the conversation, so to act on it,
say which findings — "fix the first two" — and the fixes happen in the main
session, which knows what you were trying to do. The command never edits and
never commits.

### The agents

| Agent | Role |
|---|---|
| `wtf-change-reviewer` | scope, tests, lint, the full review |
| `wtf-lens` | one dimension only; dispatched six times by `--deep` |
| `wtf-refuter` | tries to kill a single finding |

All three are read-only — no `Edit`, no `Write`, and no ability to spawn an agent
that has them. Edits only ever happen in the main session, one approval at a time.

### Tuning it

- `reference/code-review-checklist.md` sets the priority order. The reviewer
  reads the deployed copy at review time, so editing it changes behaviour
  without touching an agent definition.
- A project's own `REVIEW.md`, `AGENTS.md` or `CLAUDE.md` wins where it conflicts.
  `REVIEW.md` is the name Anthropic's own code review reads.
- The six `--deep` rubrics live in the command, not in `wtf-lens`, so they can be
  retuned without editing an agent.

### Cost

`--deep` spawns one reviewer, six lenses, and one refuter per verified finding —
tens of agents on a real branch. It announces each fan-out before spawning it, so
the spend can be refused. For very large diffs, the built-in `/code-review ultra`
is the maintained alternative.
