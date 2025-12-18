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

cp ~/src/dotfiles/Dotfiles/emacs.d/early-init.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/init.el ~/.emacs.d/.

# This file will always be empty in git, but might have local changes that
# we do not want to overwrite
touch ~/src/dotfiles/Dotfiles/emacs.d/my-customized.el

# claude code
if [ ! -d ~/.claude ]; then
    mkdir ~/.claude
fi
if [ ! -d ~/.claude/commands ]; then
    mkdir ~/.claude/commands
fi
if [ ! -d ~/.claude/hooks ]; then
    mkdir ~/.claude/hooks
fi


cp ~/src/dotfiles/Dotfiles/claude/settings.json ~/.claude/settings.json
cp ~/src/dotfiles/Dotfiles/claude/commands/* ~/.claude/commands/.
cp ~/src/dotfiles/Dotfiles/claude/hooks/* ~/.claude/hooks/.
cp ~/src/dotfiles/Dotfiles/claude/CLAUDE.md ~/.claude/CLAUDE.md

echo "Successfully synced dotfiles."
echo "If this is a work computer ensure that the correct email is being used in .gitconfig"
