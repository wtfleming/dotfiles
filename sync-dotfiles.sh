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

if [ ! -d ~/.emacs.d/custom ]; then
    mkdir ~/.emacs.d/custom
fi

cp ~/src/dotfiles/Dotfiles/emacs.d/early-init.el ~/.emacs.d/.
cp ~/src/dotfiles/Dotfiles/emacs.d/init.el ~/.emacs.d/.

# This file will always be empty in git, but might have local changes that
# we do not want to overwrite
touch ~/src/dotfiles/Dotfiles/emacs.d/my-customized.el

cp ~/src/dotfiles/Dotfiles/emacs.d/custom/abbrev-defs ~/.emacs.d/custom/.

echo "Successfully synced dotfiles"


# write a bash script to copy every file except one names sync-dotfiles.sh to ~/tmp

# *** (Claude claude-3-7-sonnet-20250219)
# ```bash
# #!/bin/bash

# # Create ~/tmp directory if it doesn't exist
# mkdir -p ~/tmp

# # Copy all files except sync-dotfiles.sh to ~/tmp
# for file in *; do
#     if [ "$file" != "sync-dotfiles.sh" ] && [ -f "$file" ]; then
#         cp "$file" ~/tmp/
#     fi
# done

# echo "Files copied to ~/tmp (excluding sync-dotfiles.sh)"
# ```


# write a bash script to copy every file in ~foo except one names sync-dotfiles.sh to ~/tmp

# *** (Claude claude-3-7-sonnet-20250219)
# ```bash
# #!/bin/bash

# # Create ~/tmp directory if it doesn't exist
# mkdir -p ~/tmp

# # Copy all files from ~foo to ~/tmp except sync-dotfiles.sh
# for file in ~foo/*; do
#   if [ "$(basename "$file")" != "sync-dotfiles.sh" ]; then
#     cp -a "$file" ~/tmp/
#   fi
# done
# ```
