#!/bin/bash
set -e

# Copy dotfiles
# it might make more sense to create symlinks instead of copying the files?
# Also could use something like GNU Stow, but for now just doing this

usage() {
    echo "Usage: $(basename "$0") [-q|--quiet]"
    echo
    echo "Copies the dotfiles in this repo into \$HOME, printing each command"
    echo "as it runs."
    echo
    echo "  -q, --quiet   only print errors"
    echo "  -h, --help    show this help"
}

quiet=0
while [ $# -gt 0 ]; do
    case "$1" in
        -q|--quiet) quiet=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Error: unknown option: $1" >&2; usage >&2; exit 1 ;;
    esac
    shift
done

# Print a command, then run it. Globs are already expanded by the time we get
# here, so the words printed are the ones that run. Arguments are joined with
# spaces and not re-quoted, so a path containing spaces would print ambiguously.
run() {
    [ "$quiet" -eq 1 ] || echo "+ $*"
    "$@"
}

if [ ! -d ~/bin ]; then
    echo "Error: ~/bin directory does not exist" >&2
    exit 1
fi

if [ ! -d ~/src/dotfiles ]; then
    echo "Error: ~/src/dotfiles directory does not exist" >&2
    exit 1
fi

# Shell scripts
run cp ~/src/dotfiles/bin/* ~/bin/.


# Git
run cp ~/src/dotfiles/Dotfiles/git-completion.bash ~/.git-completion.bash
run cp ~/src/dotfiles/Dotfiles/git-completion.zsh ~/.git-completion.zsh
run cp ~/src/dotfiles/Dotfiles/git-prompt.sh ~/.git-prompt.sh
run cp ~/src/dotfiles/Dotfiles/gitexcludes ~/.gitexcludes
# gitconfig carries the personal identity and an includeIf for ~/src/work/, so
# work identity lives in the un-synced ~/.gitconfig-work and this overwrite is
# safe on a work computer.
run cp ~/src/dotfiles/Dotfiles/gitconfig ~/.gitconfig

# tmux
run cp ~/src/dotfiles/Dotfiles/tmux.conf ~/.tmux.conf

# zsh
run cp ~/src/dotfiles/Dotfiles/zshrc ~/.zshrc

# ssh
if [ ! -d ~/.ssh ]; then
    run mkdir ~/.ssh
fi
run cp ~/src/dotfiles/Dotfiles/ssh/config ~/.ssh/config

# emacs
if [ ! -d ~/.emacs.d ]; then
    run mkdir ~/.emacs.d
fi

if [ ! -d ~/.emacs.d/wtf-elisp ]; then
    run mkdir ~/.emacs.d/wtf-elisp
fi

run cp ~/src/dotfiles/Dotfiles/emacs.d/early-init.el ~/.emacs.d/.
run cp ~/src/dotfiles/Dotfiles/emacs.d/init.el ~/.emacs.d/.

# vendored assets for markdown-preview styling and syntax highlighting
# (see markdown-css-paths and markdown-xhtml-header-content in init.el)
run cp ~/src/dotfiles/Dotfiles/emacs.d/github-markdown.css ~/.emacs.d/.
run cp ~/src/dotfiles/Dotfiles/emacs.d/highlight-github.css ~/.emacs.d/.
run cp ~/src/dotfiles/Dotfiles/emacs.d/highlight.min.js ~/.emacs.d/.
run cp ~/src/dotfiles/Dotfiles/emacs.d/lisp.min.js ~/.emacs.d/.

# init.el adds this dir to load-path
run cp ~/src/dotfiles/Dotfiles/emacs.d/wtf-elisp/*.el ~/.emacs.d/wtf-elisp/.

# This file will always be empty in git, but might have local changes that
# we do not want to overwrite
run touch ~/src/dotfiles/Dotfiles/emacs.d/my-customized.el

# init.el loads this file, so make sure it exists in ~/.emacs.d on a fresh
# machine. Never cp it — the deployed copy holds machine-local customizations.
run touch ~/.emacs.d/my-customized.el

# claude code
if [ ! -d ~/.claude ]; then
    run mkdir ~/.claude
fi
if [ ! -d ~/.claude/hooks ]; then
    run mkdir ~/.claude/hooks
fi
if [ ! -d ~/.claude/skills ]; then
    run mkdir ~/.claude/skills
fi
if [ ! -d ~/.claude/scripts ]; then
    run mkdir ~/.claude/scripts
fi
if [ ! -d ~/.claude/reference ]; then
    run mkdir ~/.claude/reference
fi
if [ ! -d ~/.claude/agents ]; then
    run mkdir ~/.claude/agents
fi
if [ ! -d ~/.claude/commands ]; then
    run mkdir ~/.claude/commands
fi

run cp ~/src/dotfiles/Dotfiles/claude/settings.json ~/.claude/settings.json
run cp -r ~/src/dotfiles/Dotfiles/claude/hooks/. ~/.claude/hooks/
run cp -r ~/src/dotfiles/Dotfiles/claude/skills/. ~/.claude/skills/
run cp -r ~/src/dotfiles/Dotfiles/claude/scripts/. ~/.claude/scripts/
run cp ~/src/dotfiles/Dotfiles/claude/CLAUDE.global.md ~/.claude/CLAUDE.md
run cp -r ~/src/dotfiles/Dotfiles/claude/reference/. ~/.claude/reference/
run cp -r ~/src/dotfiles/Dotfiles/claude/agents/. ~/.claude/agents/
run cp -r ~/src/dotfiles/Dotfiles/claude/commands/. ~/.claude/commands/


if [ "$quiet" -eq 0 ]; then
    echo "Successfully synced dotfiles."
    echo "If this is a work computer, work identity belongs in ~/.gitconfig-work (see Dotfiles/gitconfig)"
fi
