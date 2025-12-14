#!/bin/bash

brew install tmux wget htop watchman coreutils p7zip tree ncdu ripgrep cmake

# ---------- Build emacs on an ARM Mac from source
# Note: if building for emacs 29 need to brew install jansson, but not for 30 and newer
brew install gcc libgccjit make gnutls texinfo autoconf pkg-config tree-sitter tree-sitter-cli

brew install ispell

# Emacs markdown-mode uses this to preview markdown
brew install pandoc

brew install uv

brew install shellcheck

# YAML language server
brew install yaml-language-server

brew install awscli

brew install coreutils curl git

# Rust
brew install rust-analyzer
