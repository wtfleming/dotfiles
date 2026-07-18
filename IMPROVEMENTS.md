# Dotfiles Repo — Problems & Improvements

A prioritized, actionable task list for Claude Code to implement later. Each task
has a problem statement, the relevant files, a proposed fix, and acceptance criteria.

**Do not implement these yet** — this file is the backlog. Work top-down; the
High-priority section contains correctness bugs and gaps where the repo and the
deployed state disagree.

---

## High priority (correctness bugs / things that don't work)

### 4. `my-customized.el` is never deployed, but `init.el` hard-loads it
- **Problem:** `init.el` sets `custom-file` to `~/.emacs.d/my-customized.el` and then
  calls `(load custom-file)` unconditionally (init.el:58–59). `sync-dotfiles.sh` only
  `touch`es the **repo** copy (`Dotfiles/emacs.d/my-customized.el`) and never creates
  the file in `~/.emacs.d/`. On a fresh machine, emacs errors at startup with
  `file-missing`.
- **Files:** `sync-dotfiles.sh`, `Dotfiles/emacs.d/init.el` (and `init.org`, its source)
- **Fix:** Either add `touch ~/.emacs.d/my-customized.el` to the sync script (do NOT
  `cp` — the deployed file holds machine-local customizations that must not be
  overwritten), or make the load tolerant: `(load custom-file :no-error)`. Doing both
  is reasonable. Apply the same change to `init.org` so the two stay in step.
- **Acceptance:** On a machine with no `~/.emacs.d/my-customized.el`, emacs starts
  cleanly after `sync-dotfiles.sh`; existing local customizations are never clobbered
  by re-running sync.

### 5. tmux launcher scripts have no shebang (shellcheck SC2148)
- **Problem:** `bin/tmux-homn`, `bin/tmux-onecc`, `bin/tmux-tokubai` start directly
  with a command and have no `#!` line. They also use the
  `cmd; if [ "$?" -eq 1 ]` antipattern instead of testing the command directly.
- **Files:** `bin/tmux-homn`, `bin/tmux-onecc`, `bin/tmux-tokubai`
- **Fix:** Add `#!/bin/bash` (or `#!/bin/sh`). Replace
  `tmux has-session -t x; if [ "$?" -eq 1 ]` with
  `if ! tmux has-session -t x 2>/dev/null; then`.
- **Acceptance:** `shellcheck bin/*` is clean.

### 6. Enable tmux focus-events for Claude Code focus tracking
- **Problem:** When Claude Code starts inside tmux it prints
  `tmux focus-events off · add 'set -g focus-events on' to ~/.tmux.conf and reattach
  for focus tracking`. `Dotfiles/tmux.conf` has no `focus-events` setting, so tmux
  doesn't forward terminal focus in/out events. This also affects this repo's claude
  hooks (`needs-permission.sh` / `notify-ready.sh`), which already special-case tmux
  sessions — focus tracking makes the "is the terminal focused" detection accurate.
- **Files:** `Dotfiles/tmux.conf`
- **Fix:** Add `set -g focus-events on` to `Dotfiles/tmux.conf`, then re-run
  `sync-dotfiles.sh` and reattach (or `tmux source-file ~/.tmux.conf`).
- **Acceptance:** Starting Claude Code in tmux no longer shows the focus-events
  warning; `tmux show-options -g focus-events` reports `on`.

### 7. `install-dependencies-macos.sh` is missing tools the configs depend on
- **Problem:** Runtime deps used by this repo's own scripts are not installed:
  - `terminal-notifier` — required by `Dotfiles/claude/hooks/{needs-permission,notify-ready}.sh`
  - `jq` — required by `Dotfiles/claude/scripts/status-line.sh`
  - `ollama` (and optionally `glow`) — required by `bin/wtf-llm-summarize` / `wtf-llm-mindmap`
  - Tools the global `Dotfiles/claude/CLAUDE.md` tells Claude are available but the
    script never installs: `imagemagick` (magick), `node`, `gh`, `docker`
  - Script hygiene: no `set -e`, and `coreutils` is installed twice (lines 3 and 23)
- **Files:** `install-dependencies-macos.sh`
- **Fix:** Add `brew install terminal-notifier jq` and the others as desired; dedupe
  `coreutils`; add `set -e` (or `set -euo pipefail`).
- **Acceptance:** A fresh machine that runs install + sync has working claude hooks,
  status line, and llm helper scripts, and every tool the global CLAUDE.md advertises.

---

## Medium priority (robustness / maintainability)

### 8. Make the sync strategy safer (symlinks or backups)
- **Problem:** `sync-dotfiles.sh` blindly `cp`s over existing files with no backup,
  no dry-run, and no diff. Edits made to deployed files don't flow back to the repo
  (the script's own comment notes this). `cp bin/*` would also break if `bin/` ever
  gains a subdirectory.
- **Files:** `sync-dotfiles.sh`
- **Fix options (pick one, document choice):**
  - Switch to symlinks (GNU Stow, or a small symlink helper) so the repo is the
    single source of truth.
  - Keep `cp` but add: `--dry-run` flag, timestamped backups of overwritten files,
    and `cp -R`/explicit file lists instead of bare globs.
- **Acceptance:** Running sync twice is idempotent and never silently destroys local edits.

### 9. gitconfig / shell email handling for work machines
- **Problem:** Personal name+email are hardcoded in `Dotfiles/gitconfig`. The sync
  script has a TODO worrying about clobbering a work email on work machines.
- **Files:** `Dotfiles/gitconfig`, `sync-dotfiles.sh`
- **Fix:** Use git conditional includes:
  ```ini
  [includeIf "gitdir:~/src/work/"]
      path = ~/.gitconfig-work
  ```
  Keep personal identity in the main gitconfig, work identity in an un-synced
  `~/.gitconfig-work`.
- **Acceptance:** Work repos pick up the work email automatically with no manual step;
  identity is defined in exactly one place.

### 10. Hardcoded version numbers in `zshrc` PATH entries will rot
- **Problem:** `Dotfiles/zshrc` pins `apache-maven-3.6.3`, `emacs-30.2/src`,
  `postgresql@16`, `spark`, `anaconda3` in `PATH`. These break on upgrade and add
  dead PATH entries on machines that don't have them.
- **Files:** `Dotfiles/zshrc`
- **Fix:** Guard each with an existence check, e.g.
  `[ -d "$HOME/bin/emacs-30.2/src" ] && path+=(...)`, or glob the version, or move
  rarely-used ones behind the `~/.work` / local include.
- **Acceptance:** A machine missing maven/spark/anaconda has a clean PATH with no
  nonexistent dirs.

### 11. Outdated model id pinned in a slash command
- **Problem:** `Dotfiles/claude/commands/deep-analysis.md` frontmatter pins
  `model: claude-opus-4-1-20250805`, an older model.
- **Files:** `Dotfiles/claude/commands/deep-analysis.md`
- **Fix:** Update to a current model id (e.g. `claude-opus-4-8` / `opus`) or drop the
  pin to inherit the session model.
- **Acceptance:** Command runs on a current model.

### 12. `.gitignore` is minimal
- **Problem:** Root `.gitignore` only ignores `.DS_Store`. Local-only artifacts like
  `Dotfiles/emacs.d/my-customized.el` (created/touched by sync, meant to hold local
  changes) risk being committed. `.claude/settings.local.json` exists at the repo
  root and is only kept out of git by the global excludes file — which item 1 shows
  is not actually wired up, so on a fresh clone it could be committed by accident.
- **Files:** `.gitignore`, `sync-dotfiles.sh`
- **Fix:** Confirm whether `my-customized.el` should be ignored (it's `touch`ed by
  sync to stay empty in git). Add `.claude/settings.local.json` to the root
  `.gitignore` so it's protected regardless of global git config.
- **Acceptance:** Local-only files cannot be accidentally committed, even on a fresh
  clone with default git config.

### 13. No CI to enforce the repo's own conventions
- **Problem:** `CLAUDE.md` requires scripts to be shellcheck-clean, but nothing
  enforces it — regressions only surface when someone remembers to run `shellcheck`
  locally (item 5 shows this already slipped).
- **Files:** `.github/workflows/` (new)
- **Fix:** Add a small GitHub Actions workflow that runs
  `shellcheck sync-dotfiles.sh install-dependencies-macos.sh bin/* Dotfiles/claude/hooks/*.sh Dotfiles/claude/scripts/*.sh`
  on push/PR. Optionally also validate `Dotfiles/claude/settings.json` with `jq empty`.
- **Acceptance:** A PR introducing a shellcheck error or invalid settings JSON fails CI.

---

## Low priority (cleanup / documentation)

### 14. README is nearly empty
- **Problem:** `README.md` is 3 lines and only mentions the git completion source.
- **Fix:** Document: what the repo contains, how to bootstrap a new machine
  (`install-dependencies-macos.sh` then `sync-dotfiles.sh`), the work-email caveat,
  and the sync strategy. Reference `CLAUDE.md`.
- **Acceptance:** A new machine can be set up by following the README alone.

### 15. Dead / archived scripts
- **Problem:** `old-scripts/` holds unused EMR/EC2 helpers; `bin/empty-directories`
  and the tmux scripts are project-specific (homn/onecc/tokubai).
- **Fix:** Confirm `old-scripts/` is intentionally archived (add a one-line README in
  that dir), and consider whether the project-specific tmux scripts belong in this
  public repo.
- **Acceptance:** Intent of each non-synced script is documented.

### 16. Stale machine-setup notes
- **Problem:** `setup-ubuntu-18-04.org` documents Ubuntu 18.04, which reached end of
  standard support in 2023; `raspbian.org` may be similarly dated. Stale docs in a
  public repo suggest procedures that no longer work.
- **Files:** `setup-ubuntu-18-04.org`, `raspbian.org`, `macos.org`
- **Fix:** Review each: update, mark as historical at the top of the file, or move to
  an archive dir alongside `old-scripts/`.
- **Acceptance:** Every setup doc is either current or explicitly labeled historical.

### 17. Commented-out cruft across shell configs
- **Problem:** `zshrc` carries large blocks of commented-out history (old
  java/nvm/android/rvm lines).
- **Fix:** Prune dead comments; keep only ones with future value. Low risk, do last.
- **Acceptance:** Shell configs are readable; git history preserves anything removed.

### 18. No LICENSE
- **Problem:** Public dotfiles repo with no license file.
- **Fix:** Add one (e.g. MIT) if you want others to reuse it. Optional.

---

## Notes / things verified as OK
- `sync-dotfiles.sh` passes `shellcheck` cleanly, as do the claude hooks
  (`needs-permission.sh`, `notify-ready.sh`), `status-line.sh`, and
  `install-dependencies-macos.sh`.
- `Dotfiles/ssh/config` and `Dotfiles/claude/settings.json` contain no secrets.
- `gitexcludes` already lists `.claude/settings.local.json` and common editor junk.
- `Dotfiles/claude/settings.json` model (`opus`) and hook/statusline paths match what
  `sync-dotfiles.sh` deploys.
