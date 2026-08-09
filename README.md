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

It prints each command as it runs. The recursive copies into `~/.claude/` print
as a single line each, so the output is a trace of commands rather than a
per-file listing. Pass `-q` / `--quiet` to silence it and print only errors.

If you add a new config file under `Dotfiles/`, also add a copy step to
`sync-dotfiles.sh`, or it will never be deployed.

## Work machines

`Dotfiles/gitconfig` hardcodes personal identity (name, email). On a work
machine, override it after syncing — e.g. with a local include that is not
tracked here — rather than editing the deployed `~/.gitconfig` (a re-sync
would clobber it).

## Emacs server

`Dotfiles/emacs.d/init.org` starts the Emacs server at startup, so `emacsclient`
can reach a running Emacs:

```elisp
(require 'server)
(unless (server-running-p)
  (server-start))
```

The guard matters: `server-start` in a second Emacs would delete the first one's
socket and take over the name.

### Implications

- **One server per user, and the first Emacs to start owns it.** Later Emacs
  processes see the socket and skip `server-start`, so `emacsclient` always talks
  to the *first* Emacs — not necessarily the window you are looking at.
- **The socket is a remote-eval channel.** It lives at
  `$TMPDIR/emacs$UID/server` with owner-only permissions on both the socket and
  its directory. Anything that can open it can evaluate arbitrary Elisp in your
  Emacs, with your privileges. Don't loosen those permissions and don't switch to
  a TCP socket.
- **A crash can strand the socket.** `server-running-p` then reports a server
  that isn't there and nothing starts. Clear it with `M-x server-force-delete`,
  then `M-x server-start`.
- **`emacsclient` is not on `PATH` here.** This is a from-source Emacs, which
  ships it next to the binary rather than installing it:
  `~/bin/emacs-30.2/lib-src/emacsclient`.

### Working with it

```sh
EC=~/bin/emacs-30.2/lib-src/emacsclient
"$EC" --eval t             # is a server up? prints t
"$EC" -n FILE              # open FILE in the running Emacs, return immediately
"$EC" FILE                 # ...and block until you finish with C-x #
"$EC" --eval '(org-roam-db-sync)'
```

Without `-n`, `emacsclient` waits — that is what makes it usable as an `$EDITOR`.
`Dotfiles/zshrc` sets `GIT_EDITOR=emacs`, which still launches a fresh Emacs per
commit; point it at `emacsclient` if you would rather reuse the running one.

The server is a prerequisite for `~/src/wtf-wiki/bin/sync-emacs`, which refreshes
org-roam's database after files are edited outside Emacs. Because org-roam is
lazy-loaded, `init.org` also pulls it in on a 2-second idle timer — otherwise
`org-roam-directory` is unbound in a fresh session and `sync-emacs` skips rather
than syncing.

## Claude Code

`Dotfiles/claude/` deploys to `~/.claude/` and includes `/wtf-review-changes`, a
fresh-context code review. See [`Dotfiles/claude/README.md`](Dotfiles/claude/README.md).

## Layout

See `CLAUDE.md` for the full layout and repo conventions. Highlights:

- `Dotfiles/` — shell, git, tmux, ssh, emacs, and Claude Code configs
- `bin/` — helper scripts deployed to `~/bin/`
- `*.org` — machine setup notes

## Credits

`Dotfiles/git-completion.*` and `git-prompt.sh` come from
https://github.com/git/git/blob/master/contrib/completion/
