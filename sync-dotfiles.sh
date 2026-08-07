#!/bin/bash
set -e

# Copy dotfiles
# it might make more sense to create symlinks instead of copying the files?
# Also could use something like GNU Stow, but for now just doing this

if [ ! -d ~/bin ]; then
    echo "Error: ~/bin directory does not exist" >&2
    exit 1
fi

if [ ! -d ~/src/dotfiles ]; then
    echo "Error: ~/src/dotfiles directory does not exist" >&2
    exit 1
fi

# Shell scripts
cp ~/src/dotfiles/bin/* ~/bin/.


# Git
cp ~/src/dotfiles/Dotfiles/git-completion.bash ~/.git-completion.bash
cp ~/src/dotfiles/Dotfiles/git-completion.zsh ~/.git-completion.zsh
cp ~/src/dotfiles/Dotfiles/git-prompt.sh ~/.git-prompt.sh
cp ~/src/dotfiles/Dotfiles/gitexcludes ~/.gitexcludes
# TODO on a work computer I likely have my email address in .gitconfig set to
# my work email, don't want to copy over if that is the case?
# Find a better way of handling this
#  one thing I could do is cd /path/to/repo && git config --local github.email EMAIL
#  for work repos
cp ~/src/dotfiles/Dotfiles/gitconfig ~/.gitconfig

# tmux
cp ~/src/dotfiles/Dotfiles/tmux.conf ~/.tmux.conf

# zsh
cp ~/src/dotfiles/Dotfiles/zshrc ~/.zshrc

# ssh
if [ ! -d ~/.ssh ]; then
    mkdir ~/.ssh
fi
cp ~/src/dotfiles/Dotfiles/ssh/config ~/.ssh/config

# emacs
if [ ! -d ~/.emacs.d ]; then
    mkdir ~/.emacs.d
fi

if [ ! -d ~/.emacs.d/wtf-elisp ]; then
    mkdir ~/.emacs.d/wtf-elisp
fi

cp ~/src/dotfiles/Dotfiles/emacs.d/early-init.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/init.el ~/.emacs.d/.

# vendored assets for markdown-preview styling and syntax highlighting
# (see markdown-css-paths and markdown-xhtml-header-content in init.el)
cp ~/src/dotfiles/Dotfiles/emacs.d/github-markdown.css ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/highlight-github.css ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/highlight.min.js ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/lisp.min.js ~/.emacs.d/.

# init.el adds this dir to load-path
cp ~/src/dotfiles/Dotfiles/emacs.d/wtf-elisp/*.el ~/.emacs.d/wtf-elisp/.

# This file will always be empty in git, but might have local changes that
# we do not want to overwrite
touch ~/src/dotfiles/Dotfiles/emacs.d/my-customized.el

# init.el loads this file, so make sure it exists in ~/.emacs.d on a fresh
# machine. Never cp it — the deployed copy holds machine-local customizations.
touch ~/.emacs.d/my-customized.el

# claude code
if [ ! -d ~/.claude ]; then
    mkdir ~/.claude
fi
if [ ! -d ~/.claude/hooks ]; then
    mkdir ~/.claude/hooks
fi
if [ ! -d ~/.claude/skills ]; then
    mkdir ~/.claude/skills
fi
if [ ! -d ~/.claude/scripts ]; then
    mkdir ~/.claude/scripts
fi
if [ ! -d ~/.claude/my-agents ]; then
    mkdir ~/.claude/my-agents
fi
if [ ! -d ~/.claude/agents ]; then
    mkdir ~/.claude/agents
fi
if [ ! -d ~/.claude/commands ]; then
    mkdir ~/.claude/commands
fi

cp ~/src/dotfiles/Dotfiles/claude/settings.json ~/.claude/settings.json
cp ~/src/dotfiles/Dotfiles/claude/hooks/* ~/.claude/hooks/.
cp -r ~/src/dotfiles/Dotfiles/claude/skills/* ~/.claude/skills/.
cp ~/src/dotfiles/Dotfiles/claude/scripts/* ~/.claude/scripts/.
cp ~/src/dotfiles/Dotfiles/claude/CLAUDE.md ~/.claude/CLAUDE.md
cp ~/src/dotfiles/Dotfiles/claude/my-agents/* ~/.claude/my-agents/.
cp -r ~/src/dotfiles/Dotfiles/claude/agents/* ~/.claude/agents/.
cp -r ~/src/dotfiles/Dotfiles/claude/commands/* ~/.claude/commands/.


echo "Successfully synced dotfiles."
echo "If this is a work computer ensure that the correct email is being used in .gitconfig"
