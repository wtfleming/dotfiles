#!/bin/bash

# Simple status line - shows model, git repo/branch and context usage

data=$(cat)

# Get model name
model=$(echo "$data" | jq -r '.model.display_name // .model.id // "unknown"')

# Get git repo and branch, if we're in a repo at all. One rev-parse gives both:
# line 1 is the repo root, line 2 the branch (literal "HEAD" when detached).
cwd=$(echo "$data" | jq -r '.workspace.current_dir // .cwd // "."')
git_info=""
if git_out=$(git -C "$cwd" rev-parse --show-toplevel --abbrev-ref HEAD 2>/dev/null); then
    repo=$(basename "$(echo "$git_out" | sed -n 1p)")
    branch=$(echo "$git_out" | sed -n 2p)
    if [ "$branch" = "HEAD" ]; then
        branch=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null || echo "detached")
    fi
    git_info="${repo}:${branch} | "
fi

# Get context info
max_ctx=$(echo "$data" | jq -r '.context_window.context_window_size // 200000')
used_pct=$(echo "$data" | jq -r '.context_window.used_percentage // empty')

# Color codes
BLUE='\033[34m'
RED='\033[31m'
RESET='\033[0m'

# Format context display
if [ -z "$used_pct" ] || [ "$used_pct" = "null" ]; then
    # Loading state - empty circles
    context_info="○○○○○○○○○○ loading..."
else
    pct=$(printf "%.0f" "$used_pct" 2>/dev/null || echo "$used_pct")
    [ "$pct" -gt 100 ] 2>/dev/null && pct=100

    # Calculate tokens in k
    used_k=$(( max_ctx * pct / 100 / 1000 ))
    max_k=$(( max_ctx / 1000 ))

    # Build circle bar (10 segments)
    bar=""
    filled=$(( pct / 10 ))

    # Blue by default, red when > 60%
    if [ "$pct" -gt 60 ]; then
        COLOR="$RED"
    else
        COLOR="$BLUE"
    fi

    for i in 0 1 2 3 4 5 6 7 8 9; do
        if [ "$i" -lt "$filled" ]; then
            bar="${bar}${COLOR}●${RESET}"
        else
            bar="${bar}○"
        fi
    done

    context_info="${bar} ${used_k}k/${max_k}k (${pct}% used)"
fi

# Output: Model | repo:branch | Context
printf '%b\n' "${model} | ${git_info}${context_info}"
