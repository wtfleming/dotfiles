# Dotfiles Repo — Problems & Improvements

A prioritized, actionable task list for Claude Code. Add new items here with a
problem statement, the relevant files, a proposed fix, and acceptance criteria.

The backlog is currently empty — all previously tracked items have been
implemented (see git history).

---

## Notes / things verified as OK
- `sync-dotfiles.sh` passes `shellcheck` cleanly, as do the claude hooks
  (`needs-permission.sh`, `notify-ready.sh`), `status-line.sh`, and
  `install-dependencies-macos.sh`.
- `Dotfiles/ssh/config` and `Dotfiles/claude/settings.json` contain no secrets.
- `gitexcludes` already lists `.claude/settings.local.json` and common editor junk.
- `Dotfiles/claude/settings.json` model (`opus`) and hook/statusline paths match what
  `sync-dotfiles.sh` deploys.
