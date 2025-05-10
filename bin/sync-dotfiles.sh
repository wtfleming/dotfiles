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

cp ~/src/dotfiles/bin/* ~/bin/.

# Git
cp ~/src/dotfiles/Dotfiles/git-completion.bash ~/.git-completion.bash
cp ~/src/dotfiles/Dotfiles/git-completion.zsh ~/.git-completion.zsh
cp ~/src/dotfiles/Dotfiles/git-prompt.sh ~/.git-prompt.sh
cp ~/src/dotfiles/Dotfiles/gitconfig ~/.gitconfig
cp ~/src/dotfiles/Dotfiles/gitexcludes ~/.gitexcludes

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

if [ ! -d ~/.emacs.d/custom ]; then
    mkdir ~/.emacs.d/custom
fi

cp ~/src/dotfiles/Dotfiles/emacs.d/early-init.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/init.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/my-customized.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/custom/abbrev-defs ~/.emacs.d/custom/.

echo "Successfully synced dotfiles"
