# Dotfiles Repo — Problems & Improvements

A prioritized, actionable task list for Claude Code to implement later. Each task
has a problem statement, the relevant files, a proposed fix, and acceptance criteria.

**Do not implement these yet** — this file is the backlog. Work top-down; the
High-priority section contains correctness bugs and gaps where the repo and the
deployed state disagree.

---

## High priority (correctness bugs / things that don't work)

### 1. `gitexcludes` is copied but never wired into git
- **Problem:** `sync-dotfiles.sh` copies `Dotfiles/gitexcludes` to `~/.gitexcludes`,
  but `Dotfiles/gitconfig` has no `[core] excludesfile = ~/.gitexcludes` entry, so
  the global ignore file is never used by git.
- **Files:** `Dotfiles/gitconfig`, `sync-dotfiles.sh`
- **Fix:** Add to `gitconfig`:
  ```ini
  [core]
      excludesfile = ~/.gitexcludes
  ```
- **Acceptance:** `git config --get core.excludesfile` returns `~/.gitexcludes` after sync.

### 2. Bash config files exist in the repo but are never synced
- **Problem:** `Dotfiles/bashrc`, `bash_profile`, `bash_aliases`, `bash_logout`
  are tracked but `sync-dotfiles.sh` only copies `zshrc`. A bash login would not
  pick up any of these.
- **Files:** `sync-dotfiles.sh`, `Dotfiles/bash*`
- **Fix:** Either (a) add copy steps for the bash files, or (b) if zsh is the only
  shell in use, move the bash files to an `archive/`/`old-scripts/` dir and note
  the decision in `README.md`. Decide intent first.
- **Acceptance:** Repo no longer contains tracked-but-unsynced shell configs without
  an explicit "intentionally not synced" note.

### 3. Several tracked config files are never synced
- **Problem:** These exist in the repo but `sync-dotfiles.sh` never deploys them:
  - `Dotfiles/config/clj-kondo/config.edn` → `~/.config/clj-kondo/config.edn`
  - `Dotfiles/lein/profiles.clj` → `~/.lein/profiles.clj`
  - `Dotfiles/sbtconfig` → `~/.sbt/...` (confirm target)
  - `Dotfiles/hiverc` → `~/.hiverc`
  - `Dotfiles/emacs.d/init.org` (source for init.el?) — confirm whether it should sync
  - `babashka-scripts/bb-edn/bb.edn`, `babashka-scripts/shell-env.sh`
- **Files:** `sync-dotfiles.sh`
- **Fix:** Add sync steps (with `mkdir -p` guards) for the configs that should be
  deployed; explicitly document the ones that should not.
- **Acceptance:** Every tracked config under `Dotfiles/` is either synced or has a
  documented reason it isn't.

### 4. tmux launcher scripts have no shebang (shellcheck SC2148)
- **Problem:** `bin/tmux-homn`, `bin/tmux-onecc`, `bin/tmux-tokubai` start directly
  with a command and have no `#!` line. They also use the
  `cmd; if [ "$?" -eq 1 ]` antipattern instead of testing the command directly.
- **Files:** `bin/tmux-homn`, `bin/tmux-onecc`, `bin/tmux-tokubai`
- **Fix:** Add `#!/bin/bash` (or `#!/bin/sh`). Replace
  `tmux has-session -t x; if [ "$?" -eq 1 ]` with
  `if ! tmux has-session -t x 2>/dev/null; then`.
- **Acceptance:** `shellcheck bin/*` is clean.

### 5. Enable tmux focus-events for Claude Code focus tracking
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

### 6. `install-dependencies-macos.sh` is missing tools the configs depend on
- **Problem:** Runtime deps used by this repo's own scripts are not installed:
  - `terminal-notifier` — required by `Dotfiles/claude/hooks/{needs-permission,notify-ready}.sh`
  - `jq` — required by `Dotfiles/claude/scripts/status-line.sh`
  - `ollama` (and optionally `glow`) — required by `bin/wtf-llm-summarize` / `wtf-llm-mindmap`
- **Files:** `install-dependencies-macos.sh`
- **Fix:** Add `brew install terminal-notifier jq` and (if desired) `ollama`/`glow`.
- **Acceptance:** A fresh machine that runs install + sync has working claude hooks,
  status line, and llm helper scripts.

---

## Medium priority (robustness / maintainability)

### 7. Make the sync strategy safer (symlinks or backups)
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

### 8. gitconfig / shell email handling for work machines
- **Problem:** Personal name+email are hardcoded in `Dotfiles/gitconfig` AND exported
  in `Dotfiles/bashrc` (`GIT_AUTHOR_EMAIL` etc.). The sync script has a TODO worrying
  about clobbering a work email. Two sources of truth for identity.
- **Files:** `Dotfiles/gitconfig`, `Dotfiles/bashrc`, `sync-dotfiles.sh`
- **Fix:** Use git conditional includes:
  ```ini
  [includeIf "gitdir:~/src/work/"]
      path = ~/.gitconfig-work
  ```
  Keep personal identity in the main gitconfig, work identity in an un-synced
  `~/.gitconfig-work`. Remove the duplicate `GIT_AUTHOR_*` exports from `bashrc`
  (let gitconfig own identity), or document why both exist.
- **Acceptance:** Work repos pick up the work email automatically with no manual step;
  identity is defined in exactly one place.

### 9. Hardcoded version numbers in `zshrc` PATH entries will rot
- **Problem:** `Dotfiles/zshrc` pins `apache-maven-3.6.3`, `emacs-30.2/src`,
  `postgresql@16`, `spark`, `anaconda3` in `PATH`. These break on upgrade and add
  dead PATH entries on machines that don't have them.
- **Files:** `Dotfiles/zshrc`
- **Fix:** Guard each with an existence check, e.g.
  `[ -d "$HOME/bin/emacs-30.2/src" ] && path+=(...)`, or glob the version, or move
  rarely-used ones behind the `~/.work` / local include.
- **Acceptance:** A machine missing maven/spark/anaconda has a clean PATH with no
  nonexistent dirs.

### 10. Outdated model id pinned in a slash command
- **Problem:** `Dotfiles/claude/commands/deep-analysis.md` frontmatter pins
  `model: claude-opus-4-1-20250805`, an older model.
- **Files:** `Dotfiles/claude/commands/deep-analysis.md`
- **Fix:** Update to a current model id (e.g. `claude-opus-4-8` / `opus`) or drop the
  pin to inherit the session model.
- **Acceptance:** Command runs on a current model.

### 11. `.gitignore` is minimal
- **Problem:** Root `.gitignore` only ignores `.DS_Store`. Local-only artifacts like
  `Dotfiles/emacs.d/my-customized.el` (created/touched by sync, meant to hold local
  changes) risk being committed.
- **Files:** `.gitignore`, `sync-dotfiles.sh`
- **Fix:** Confirm whether `my-customized.el` should be ignored (it's `touch`ed by
  sync to stay empty in git). If yes, add it to `.gitignore`.
- **Acceptance:** Local-only files cannot be accidentally committed.

---

## Low priority (cleanup / documentation)

### 12. README is nearly empty
- **Problem:** `README.md` is 3 lines and only mentions the git completion source.
- **Fix:** Document: what the repo contains, how to bootstrap a new machine
  (`install-dependencies-macos.sh` then `sync-dotfiles.sh`), the work-email caveat,
  and the sync strategy. Reference `CLAUDE.md`.
- **Acceptance:** A new machine can be set up by following the README alone.

### 13. Dead / archived scripts
- **Problem:** `old-scripts/` holds unused EMR/EC2 helpers; `bin/empty-directories`
  and the tmux scripts are project-specific (homn/onecc/tokubai).
- **Fix:** Confirm `old-scripts/` is intentionally archived (add a one-line README in
  that dir), and consider whether the project-specific tmux scripts belong in this
  public repo.
- **Acceptance:** Intent of each non-synced script is documented.

### 14. Commented-out cruft across shell configs
- **Problem:** `bashrc`, `zshrc`, `bash_profile`, `bash_aliases` carry large blocks of
  commented-out history (old java/nvm/android/rvm lines).
- **Fix:** Prune dead comments; keep only ones with future value. Low risk, do last.
- **Acceptance:** Shell configs are readable; git history preserves anything removed.

### 15. No LICENSE
- **Problem:** Public dotfiles repo with no license file.
- **Fix:** Add one (e.g. MIT) if you want others to reuse it. Optional.

### 16. `bashrc` uses non-POSIX `==` in `[ ]`
- **Problem:** `[ "$(uname)" == "Darwin" ]` — works in bash but not POSIX `sh`.
- **Fix:** Use single `=`. Minor.

---

## Notes / things verified as OK
- `sync-dotfiles.sh` passes `shellcheck` cleanly.
- `Dotfiles/ssh/config` and `Dotfiles/claude/settings.json` contain no secrets.
- `gitexcludes` already lists `.claude/settings.local.json` and common editor junk.
