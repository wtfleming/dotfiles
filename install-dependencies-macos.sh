#!/bin/bash

set -euo pipefail

brew install tmux wget htop watchman coreutils p7zip tree ncdu ripgrep cmake

# ---------- Build emacs on an ARM Mac from source
# Note: if building for emacs 29 need to brew install jansson, but not for 30 and newer
# Note: tree-sitter is pinned to @0.25 because the emacs binary built here links
# libtree-sitter.0.25.dylib by soname. Plain `tree-sitter` is 0.26 upstream now;
# installing it repoints /opt/homebrew/opt/tree-sitter and emacs then fails to
# start on the missing dylib. Bump this only together with an emacs rebuild.
brew install gcc libgccjit make gnutls texinfo autoconf pkg-config tree-sitter@0.25 tree-sitter-cli

brew install ispell

# Emacs markdown-mode uses this to preview markdown
brew install pandoc

brew install uv

brew install shellcheck

# YAML language server
brew install yaml-language-server

# TypeScript language server. Claude Code's typescript-lsp plugin needs this
# binary on PATH and does not install it; without it the plugin loads with an
# "Executable not found in $PATH" error and Claude falls back to grep for
# finding definitions and references.
brew install typescript-language-server

# Structural search over a tree-sitter AST. Used by hand for review questions grep
# is bad at, such as "does this shape already exist elsewhere" -- see todo.org.
brew install ast-grep

brew install awscli

brew install curl git

# Rust
brew install rust-analyzer

# Used by the Claude Code hooks (Dotfiles/claude/hooks/*.sh)
brew install terminal-notifier

# Used by the Claude Code status line (Dotfiles/claude/scripts/status-line.sh)
brew install jq

# Used by bin/wtf-llm-summarize and bin/wtf-llm-mindmap
brew install ollama

# Tools the global claude CLAUDE.md advertises as available
brew install imagemagick

# Not advertised to Claude, but depended on anyway, so do not prune these along
# with that list: Dotfiles/claude/settings.json allowlists `node --check` and
# `gh pr merge`, and Dotfiles/emacs.d/wtf-elisp/wtf-github.el shells out to gh.
brew install node gh

# Docker Desktop (provides the docker CLI). Guarded so re-running the script
# doesn't fail on a machine where Docker.app was installed manually.
if [ ! -d /Applications/Docker.app ]; then
    brew install --cask docker
fi
