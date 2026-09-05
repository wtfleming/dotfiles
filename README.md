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

The Emacs config also expects the `wtf-wiki` repo at `~/src/wtf-wiki` — that is
org-roam's default graph (see [Emacs server](#emacs-server) below). Clone it
there too. Without it org-roam comes up pointing at a directory that does not
exist, which reports nothing and simply looks like an empty graph; `C-c n s`
switches to the personal notes graph in the meantime.

## How the sync works

`sync-dotfiles.sh` **copies** files from this repo into the home directory —
no symlinks. The repo is the source of truth: edits made to the deployed
copies in `$HOME` do not flow back. Change the file under `Dotfiles/` and
re-run the sync.

It prints each command as it runs. The recursive copies into `~/.claude/` print
as a single line each, so the output is a trace of commands rather than a
per-file listing. Pass `-q` / `--quiet` to silence it and print only errors.

If you add a new config file under `Dotfiles/`, it usually needs a copy step in
`sync-dotfiles.sh` or it will never be deployed — but not always. The six directories
under `Dotfiles/claude/` (`agents/`, `commands/`, `skills/`, `hooks/`, `scripts/`,
`reference/`) are swept recursively, so new files and whole new subdirectories deploy
with no edit at all. `CLAUDE.md` has the three copy patterns and which one applies
where.

The sync only ever copies; it never deletes. Renaming or removing a file here
leaves the old copy deployed in `$HOME`, so delete that by hand in the same
change — for `commands/`, `agents/` and `skills/` the stale copy stays
registered under its old name and goes on working.

## Work machines

`Dotfiles/gitconfig` carries the personal identity and ends with a path-scoped
include:

```ini
[includeIf "gitdir:~/src/work/"]
        path = ~/.gitconfig-work
```

Check work repos out under `~/src/work/`, and put the work identity in
`~/.gitconfig-work`:

```ini
[user]
        email = <work address>
```

That file is deliberately not tracked here and not synced, so the sync's
overwrite of `~/.gitconfig` cannot clobber it. Don't add it to this repo.

Only repos under `~/src/work/` pick it up. Everything else — this repo
included — keeps the personal identity, so a commit here stays personal even on
a work machine. Git ignores the include silently while the file is absent, so
personal machines need nothing.

Don't override identity machine-wide instead. An unconditional include applies
the work email to personal repos too, and editing the deployed `~/.gitconfig`
directly is clobbered on the next sync.

## Emacs server

`Dotfiles/emacs.d/init.org` starts the Emacs server at startup, so `emacsclient`
can reach a running Emacs:

```elisp
(defun wtf-claim-emacs-server ()
  (unless (server-running-p)
    (server-start)
    (run-with-idle-timer 2 nil (lambda () (require 'org-roam)))))

(wtf-claim-emacs-server)
(run-with-idle-timer 30 t #'wtf-claim-emacs-server)
```

### Implications

- **One server per user, and the first Emacs to start owns it.** Later Emacs
  processes see the socket and skip `server-start`, so `emacsclient` always talks
  to the *first* Emacs — not necessarily the window you are looking at.
- **Ownership can be lost, which is why the timer is there.** The owner deletes
  the socket when it exits, and an Emacs that skipped `server-start` at startup
  would otherwise serve nothing for the rest of its life. `GIT_EDITOR=emacs`
  makes that easy to hit: a commit-message Emacs that starts first claims the
  socket, then takes it away on exit. The idle re-check reclaims it.
- **The guard is not what stops a second Emacs stealing the socket** — nothing
  steals it. `server-start` on a name another Emacs already serves warns, leaves
  `server-process` nil, and changes nothing. The guard just keeps `*Warnings*`
  clean.
- **The socket is a remote-eval channel.** It lives at
  `$TMPDIR/emacs$UID/server` with owner-only permissions on both the socket and
  its directory. Anything that can open it can evaluate arbitrary Elisp in your
  Emacs, with your privileges. Don't loosen those permissions and don't switch to
  a TCP socket.
- **A socket stranded by a crash needs no cleanup.** For a local socket
  `server-running-p` opens a real connection, so it reports nil and the next
  `server-start` unlinks the leftover itself. `M-x server-force-delete` is for
  the other case — a *live* Emacs holding the name that you want to displace.
- **`emacsclient` comes from the from-source build.** It is not installed
  alongside the binary, so `Dotfiles/zshrc` puts its `lib-src` directory on
  `PATH` next to the `src` one. Both carry the Emacs version, so a new build
  means editing those two adjacent lines and nothing else.

### Working with it

```sh
emacsclient --eval t       # is a server up? prints t
emacsclient -n FILE        # open FILE in the running Emacs, return immediately
emacsclient FILE           # ...and block until you finish with C-x #
emacsclient --eval '(org-roam-db-sync)'
```

Without `-n`, `emacsclient` waits — that is what makes it usable as an `$EDITOR`.
`Dotfiles/zshrc` sets `GIT_EDITOR=emacs`, which still launches a fresh Emacs per
commit; point it at `emacsclient` if you would rather reuse the running one.

The server is a prerequisite for `~/src/wtf-wiki/bin/sync-emacs`, which refreshes
org-roam's database after files are edited outside Emacs. Because org-roam is
lazy-loaded, claiming the server also pulls it in on a 2-second idle timer —
otherwise `org-roam-directory` is unbound in a fresh session and `sync-emacs`
skips rather than syncing. That preload is deliberately tied to claiming the
socket: it is wasted work in any Emacs `emacsclient` cannot reach.

## Claude Code

`Dotfiles/claude/` deploys to `~/.claude/` and includes `/wtf-code-review`, a
fresh-context code review. See [`Dotfiles/claude/README.md`](Dotfiles/claude/README.md).

## Codex CLI

`install-dependencies-macos.sh` installs Codex through Homebrew. The footer in
`Dotfiles/codex/config.toml` shows model and reasoning effort, current directory,
git branch, context usage and capacity, and the five-hour rate limit.

Unlike the file copies above, `sync-dotfiles.sh` merges only `tui.status_line`
into `~/.codex/config.toml`, preserving local settings and comments. The merge
helper uses `uv` (installed by the dependency script) to run Python with a pinned
`tomlkit` dependency; its first run may download that dependency and Python.
Edit the repo config and sync again to change the footer. Codex's `/statusline`
picker changes the local setting, which the next sync replaces.

## Layout

See `CLAUDE.md` for the full layout and repo conventions. Highlights:

- `Dotfiles/` — shell, git, tmux, ssh, emacs, Claude Code, and Codex configs
- `bin/` — helper scripts deployed to `~/bin/`
- `*.org` — machine setup notes

## Credits

`Dotfiles/git-completion.*` and `git-prompt.sh` come from
https://github.com/git/git/blob/master/contrib/completion/
