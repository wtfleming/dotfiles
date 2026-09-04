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
- **A new file usually needs a matching copy step in `sync-dotfiles.sh`**, or it is
  never deployed — but *which* step, or whether one is needed at all, depends on how its
  directory is copied. Three patterns are in use:
  - **Copied by name** — `Dotfiles/gitconfig`, `Dotfiles/claude/settings.json`, most of
    `emacs.d/`. Add a line, and write it as `run cp ...`, not bare `cp`: `run` echoes the
    command so the sync's output accounts for every file. A bare `cp` still copies, it
    just goes unreported.
  - **Copied by glob** — `bin/*` and `emacs.d/wtf-elisp/*.el`. A new *file* there needs
    no edit. A new *directory* is where the two diverge. `bin/*` matches one, and `cp`
    without `-r` exits 1 on a directory operand, so `set -e` stops the sync there and
    nothing below it is copied — `~/.claude` included. `wtf-elisp/*.el` matches only a
    directory whose own name ends `.el`, so an ordinary subdirectory there is skipped
    without a word instead. The abort itself is not silent — `cp` prints its own error
    and an `EXIT` trap names the step it stopped at — but neither tells you which of the
    later steps still need running.
  - **Swept whole** — `hooks/`, `skills/`, `scripts/`, `reference/`, `agents/` and
    `commands/` under `Dotfiles/claude/`, each copied as `cp -r .../X/. ~/.claude/X/`.
    New files *and* whole new subdirectories deploy with no change to the sync, so
    adding a skill, agent or command needs nothing here.
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
- `Dotfiles/claude/tests/` holds checks that run in CI against the repo copy of a script.
  It is deliberately **not** deployed — nothing in `~/.claude` runs them, and a test suite
  in the home directory is cruft. It is therefore also not one of the swept directories
  above; adding a file there needs no sync change *because* nothing syncs it.
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
  `shellcheck sync-dotfiles.sh install-dependencies-macos.sh bin/* Dotfiles/claude/hooks/*.sh Dotfiles/claude/scripts/*.sh Dotfiles/claude/skills/*/scripts/*.sh Dotfiles/claude/tests/*.sh`
- CI also parses the YAML frontmatter of every `SKILL.md`, agent and command. A header
  that does not parse registers wrong or not at all, and nothing else notices —
  `shellcheck` skips markdown and `jq` only sees `settings.json`.
- CI runs the smoke tests in `Dotfiles/claude/tests/`, the only steps that *execute*
  anything. Each one is there because its script fails in a way no static check reaches.
  `resolve-scope.sh` is almost entirely branching and is the single authority on what every
  review reads, so a wrong answer there is a confident report about the wrong code.
  `baseline-worktree.sh` builds and destroys git worktrees, so its failure modes are a
  guard that does not stop the run and half a pair left registered for the next run to trip
  over. `publish-verify-section.sh` rewrites a public PR description, so its failure mode is
  deleting prose the author wrote, somewhere that notifies watchers and cannot be taken
  back — it lived as a fenced block inside `reference/github-publishing.md` until the
  shellcheck glob and a test could reach it. A new test file here needs a step added to
  `.github/workflows/lint.yml` — the shellcheck glob picks it up, but nothing runs it
  otherwise.
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
  `Dotfiles/gitconfig`, which ends with an `includeIf "gitdir:~/src/work/"` pointing
  at `~/.gitconfig-work`. That file is **not** in this repo and must never be added
  to it — it is the one place a work email is allowed to live. Work repos checked
  out under `~/src/work/` pick it up; everything else, this repo included, keeps the
  personal identity, so a commit here stays personal even on a work machine. Git
  ignores the include silently when the file is absent, so personal machines need
  nothing. The `~/src/work/` prefix is the convention the config keys on — a work
  checkout outside it silently gets the personal identity instead.
- This is a **public** repo — don't commit credentials, tokens, internal hostnames,
  or work email addresses.
