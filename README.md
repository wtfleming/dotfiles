dotfiles
========

Personal dotfiles for macOS. Config files live under `Dotfiles/`, helper
scripts under `bin/`, and both are deployed to `$HOME` by `sync-dotfiles.sh`.

## Setting up a new Mac

```sh
git clone git@github.com:wtfleming/dotfiles.git ~/src/dotfiles
cd ~/src/dotfiles
./install-dependencies-macos.sh   # Homebrew packages (assumes brew is installed)
./sync-dotfiles.sh                # copy configs into $HOME
```

## How the sync works

`sync-dotfiles.sh` **copies** files from this repo into the home directory —
no symlinks. The repo is the source of truth: edits made to the deployed
copies in `$HOME` do not flow back. Change the file under `Dotfiles/` and
re-run the sync.

If you add a new config file under `Dotfiles/`, also add a copy step to
`sync-dotfiles.sh`, or it will never be deployed.

## Work machines

`Dotfiles/gitconfig` hardcodes personal identity (name, email). On a work
machine, override it after syncing — e.g. with a local include that is not
tracked here — rather than editing the deployed `~/.gitconfig` (a re-sync
would clobber it).

## Code review

`/wtf-review-changes` reviews recent changes in a **fresh context** — a subagent
that never sees the conversation which wrote the code, so it cannot inherit the
author's assumptions about it. It deploys to `~/.claude/`, so it works in every
repo, not just this one.

```
/wtf-review-changes                  # uncommitted, else the branch, else HEAD
/wtf-review-changes HEAD~3           # any ref, branch or path
/wtf-review-changes --fix            # then triage each finding interactively
/wtf-review-changes main --deep      # add a parallel pass per dimension
/wtf-review-changes --deep --fix     # both
```

With no flags it settles the scope, runs the project's test suite and linter,
reviews the diff against the checklist, and prints findings as Critical, Warning
or Suggestion — then stops. The reviewer has no `Edit` or `Write`, so a plain
review cannot change anything.

`--deep` adds five `wtf-lens` agents in parallel, one per dimension: correctness,
security, tests, maintainability, performance. Their reports are merged and
deduplicated with the reviewer's. There is deliberately no linter lens — the
reviewer already runs the real one.

`--fix` walks the surviving findings one at a time and offers to apply each. It
edits only what a finding names, and it never commits.

Either flag runs verification first: one `wtf-refuter` per finding, each told to
argue the finding is *wrong* and to answer refuted when unsure. Under `--deep`
alone only Critical and Warning are verified; Suggestions arrive marked
`(unverified)` rather than being dropped.

### The agents

| Agent | Role |
|---|---|
| `wtf-change-reviewer` | scope, tests, lint, the full review |
| `wtf-lens` | one dimension only; dispatched five times by `--deep` |
| `wtf-refuter` | tries to kill a single finding |

All three are read-only — no `Edit`, no `Write`, and no ability to spawn an agent
that has them. Edits only ever happen in the main session, one approval at a time.

### Tuning it

- `Dotfiles/claude/reference/code-review-checklist.md` sets the priority order.
  The reviewer reads the deployed copy at review time, so editing it changes
  behaviour without touching an agent definition.
- A project's own `REVIEW.md`, `AGENTS.md` or `CLAUDE.md` wins where it conflicts.
  `REVIEW.md` is the name Anthropic's own code review reads.
- The five `--deep` rubrics live in the command, not in `wtf-lens`, so they can be
  retuned without editing an agent.

### Cost

`--deep` spawns one reviewer, five lenses, and one refuter per verified finding —
tens of agents on a real branch. It announces each fan-out before spawning it, so
the spend can be refused. For very large diffs, the built-in `/code-review ultra`
is the maintained alternative.

## Layout

See `CLAUDE.md` for the full layout and repo conventions. Highlights:

- `Dotfiles/` — shell, git, tmux, ssh, emacs, and Claude Code configs
- `bin/` — helper scripts deployed to `~/bin/`
- `*.org` — machine setup notes

## Credits

`Dotfiles/git-completion.*` and `git-prompt.sh` come from
https://github.com/git/git/blob/master/contrib/completion/
