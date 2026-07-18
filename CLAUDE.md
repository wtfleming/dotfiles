# CLAUDE.md

Guidance for Claude Code when working in this repository.

## What this repo is

Personal dotfiles for macOS (with some Linux/Raspbian/Ubuntu setup notes). The
canonical copies of config files live under `Dotfiles/` and a few helper scripts
live under `bin/`. They are deployed to the home directory by `sync-dotfiles.sh`.

## Layout

- `Dotfiles/` — source-of-truth config files (shell, git, tmux, ssh, emacs, claude).
  - `Dotfiles/claude/` — Claude Code config: `settings.json`, `CLAUDE.md` (the
    global one deployed to `~/.claude/`), `commands/`, `hooks/`, `skills/`,
    `scripts/`, `my-agents/`.
- `bin/` — small helper scripts deployed to `~/bin/` (tmux session launchers, llm helpers).
- `docker-compose/` — misc tooling.
- `old-scripts/` — archived, unused scripts (do not deploy).
- `*.org` — machine setup notes (`macos.org`, `raspbian.org`, `setup-ubuntu-18-04.org`).
- `install-dependencies-macos.sh` — Homebrew bootstrap for a new Mac.
- `sync-dotfiles.sh` — copies files from this repo into `$HOME`.
- `IMPROVEMENTS.md` — backlog of known problems and planned improvements.

## How deployment works

`sync-dotfiles.sh` **copies** (not symlinks) files from `~/src/dotfiles` into the
home directory. Key consequences to keep in mind:

- The repo is the source of truth. Edits made to deployed files in `$HOME` do **not**
  flow back — change the file under `Dotfiles/` and re-run the sync.
- **If you add a new config file under `Dotfiles/`, you must also add a copy step to
  `sync-dotfiles.sh`**, or it will never be deployed. Several files currently violate
  this — see `IMPROVEMENTS.md`.
- `Dotfiles/emacs.d/my-customized.el` is intended to hold machine-local emacs changes;
  sync only `touch`es it so it stays empty in git.
- `Dotfiles/emacs.d/init.org` is the org source that generates `init.el`. It is
  edited and tangled in the repo and is intentionally **not** synced to `$HOME`.

## Conventions

- Shell scripts target bash/zsh on macOS. Keep them `shellcheck`-clean
  (`shellcheck sync-dotfiles.sh bin/*`).
- Prefer POSIX-compatible test syntax (`[ "$x" = "$y" ]`, not `==`) in `sh` scripts.
- Don't hardcode secrets or work-specific identity. Personal identity lives in
  `Dotfiles/gitconfig`; work machines should override via an un-synced include.
- This is a **public** repo — don't commit credentials, tokens, internal hostnames,
  or work email addresses.

## Before committing

- Run `shellcheck` on any script you touched.
- If you changed what gets deployed, verify `sync-dotfiles.sh` still references it.
- Keep `IMPROVEMENTS.md` current: check off or remove items you implement.

## Working on the backlog

`IMPROVEMENTS.md` is the prioritized task list. Implement top-down (High → Medium →
Low). Each task lists the affected files and acceptance criteria.
