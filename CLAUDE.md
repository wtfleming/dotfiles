# CLAUDE.md

Guidance for Claude Code when working in this repository.

## What this repo is

Personal dotfiles for macOS (with some Linux/Raspbian/Ubuntu setup notes). The
canonical copies of config files live under `Dotfiles/` and a few helper scripts
live under `bin/`. They are deployed to the home directory by `sync-dotfiles.sh`.

## How deployment works

`sync-dotfiles.sh` **copies** (not symlinks) files from `~/src/dotfiles` into the
home directory. Key consequences to keep in mind:

- The repo is the source of truth. Edits made to deployed files in `$HOME` do **not**
  flow back — change the file under `Dotfiles/` and re-run the sync.
- **If you add a new config file under `Dotfiles/`, you must also add a copy step to
  `sync-dotfiles.sh`**, or it will never be deployed. Write the step as
  `run cp ...`, not bare `cp` — `run` echoes the command so the sync's output
  accounts for every file. A bare `cp` still copies, it just goes unreported.
- **Deleting or renaming a file here does not remove the deployed copy** — `cp` only
  ever adds. Delete it from `$HOME` by hand as well, in the same change. Skipping that
  leaves a file nothing in the repo accounts for, which later reads as untracked local
  cruft and gets "restored", silently reversing the deletion. For a rename it is worse:
  both names stay live, and for `agents/`, `commands/` and `skills/` both stay
  registered.
- `Dotfiles/emacs.d/my-customized.el` is intended to hold machine-local emacs changes;
  sync only `touch`es it so it stays empty in git.
- `Dotfiles/emacs.d/init.org` is the org source that generates `init.el`. It is
  edited and tangled in the repo and is intentionally **not** synced to `$HOME`.
  **Nothing verifies the two agree** — CI used to re-tangle and diff, but it did
  so with whatever `emacs-nox` Ubuntu ships (29.3, against 30.2 locally), so a
  green check only meant a different Org version agreed. Re-tangle by hand
  (`M-x org-babel-tangle`) and commit `init.el` in the same change; a forgotten
  tangle ships a config the source no longer describes, silently.
- `Dotfiles/claude/CLAUDE.global.md` is deployed to `~/.claude/CLAUDE.md` — it is the
  global memory file, not documentation for this repo.
  - Its "command line tools available" list and the `brew install` lines in
    `install-dependencies-macos.sh` are a pair, and nothing checks one against the
    other. The list is deliberately **not** a mirror of the script: it carries only
    tools Claude would otherwise fail to discover or would invoke under the wrong
    name, so installing something new rarely earns a list entry — but removing or
    renaming a listed tool must prune the list in the same change, or Claude is
    told to reach for a binary that isn't there.
- `old-scripts/` is archived and deliberately **not** deployed.

## Workflow

- Work on a branch and open a PR to `main`; don't commit to `main` directly.
  Automated code review runs only on PRs, so a direct push gets no review. (The
  `lint` workflow runs either way — on every PR, and on pushes to `main`.)
- **When you act on a review comment, reply to it on the PR saying what you did,
  and resolve the thread.** A pushed fix with no reply leaves the reviewer — and
  anyone reading the PR later — to diff the branch and guess which comment it
  answered. Reply on the comment thread itself, not as a new top-level comment.
  - Verify a finding before acting on it; automated reviewers are wrong often
    enough that "it was reported" is not a reason to change code. Say in the
    reply how it was verified, not just that it was fixed.
  - Declining is a legitimate outcome. Reply with the reason and leave the
    thread **open** for a human to weigh in — resolving is for comments actually
    addressed, and self-resolving a disagreement hides it.
  - Replies are public on this repo. Say what changed and why; don't paste
    internal reasoning, paths outside the repo, or machine-specific output.

## Conventions

- Shell scripts target bash/zsh on macOS. Keep them `shellcheck`-clean — CI runs
  the same set on every push:
  `shellcheck sync-dotfiles.sh install-dependencies-macos.sh bin/* Dotfiles/claude/hooks/*.sh Dotfiles/claude/scripts/*.sh`
- Prefer POSIX-compatible test syntax (`[ "$x" = "$y" ]`, not `==`) in `sh` scripts.
- **New agents, commands and skills under `Dotfiles/claude/` take a `wtf-` prefix**,
  in the file name *and* the registered name. These share a namespace with whatever
  the current project defines in its own `.claude/`, and a collision resolves
  silently to one of them rather than erroring. Where the registered name lives
  differs by type:
  - agent — the `name:` frontmatter, which is what `subagent_type` must match
  - command — the file name; there is no `name:` field
  - skill — both the directory name and `name:`
- `reference/`, `hooks/` and `scripts/` are reached by explicit path from
  `CLAUDE.md` and `settings.json`, so they share no namespace and are not prefixed.
- Don't hardcode secrets or work-specific identity. Personal identity lives in
  `Dotfiles/gitconfig`; work machines should override via an un-synced include.
- This is a **public** repo — don't commit credentials, tokens, internal hostnames,
  or work email addresses.
