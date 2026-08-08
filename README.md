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
