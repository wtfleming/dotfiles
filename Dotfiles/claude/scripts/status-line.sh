#!/bin/bash

# Status line: model | repo:branch | PR | context bar | rate limit | lines changed
# Runs on every assistant message, so it makes exactly two forks: one jq, one git.

# Longest branch name shown before it is clipped to a leading fragment.
MAX_BRANCH=20

data=$(cat)

# One jq pass for every field we need, one per line in a fixed order. Absent or
# null becomes "" so the segments below can test for emptiness. Read with an
# empty IFS so blank lines stay blank: with IFS=$'\n' a run of empty fields
# collapses into one delimiter and every later value shifts up by a slot. The
# trailing "END" absorbs command substitution stripping trailing newlines.
fields=()
while IFS= read -r line; do fields+=("$line"); done <<EOF
$(echo "$data" | jq -r '
  [ .model.display_name // .model.id // "unknown",
    .context_window.context_window_size // 200000,
    .context_window.used_percentage // "",
    .context_window.total_input_tokens // 0,
    .workspace.current_dir // .cwd // ".",
    .pr.number // "",
    .rate_limits.five_hour.used_percentage // "",
    .cost.total_lines_added // 0,
    .cost.total_lines_removed // 0,
    "END"
  ] | .[] | tostring')
EOF

model=${fields[0]}
max_ctx=${fields[1]}
used_pct=${fields[2]}
in_tok=${fields[3]}
cwd=${fields[4]}
pr_num=${fields[5]}
rl_pct=${fields[6]}
lines_add=${fields[7]}
lines_del=${fields[8]}

# Color codes
BLUE='\033[34m'
RED='\033[31m'
RESET='\033[0m'

# --- line 1 -----------------------------------------------------------------

# Git repo and branch, if we're in a repo at all. One rev-parse gives both:
# line 1 is the repo root, line 2 the branch (literal "HEAD" when detached).
git_info=""
if git_out=$(git -C "$cwd" rev-parse --show-toplevel --abbrev-ref HEAD 2>/dev/null); then
    repo=$(basename "$(echo "$git_out" | sed -n 1p)")
    branch=$(echo "$git_out" | sed -n 2p)
    if [ "$branch" = "HEAD" ]; then
        branch=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null || echo "detached")
    fi
    if [ ${#branch} -gt $MAX_BRANCH ]; then
        branch="${branch:0:$((MAX_BRANCH - 1))}…"
    fi
    git_info=" | ${repo}:${branch}"
fi

# Open PR for this branch. Claude Code drops .pr once the PR merges or closes.
pr_info=""
if [ -n "$pr_num" ]; then
    pr_info=" | #${pr_num}"
fi

# --- line 2 -----------------------------------------------------------------

if [ -z "$used_pct" ]; then
    # Loading state - empty circles
    context_info="○○○○○○○○○○ loading..."
else
    pct=$(printf "%.0f" "$used_pct" 2>/dev/null || echo "$used_pct")
    [ "$pct" -gt 100 ] 2>/dev/null && pct=100

    # Prefer the exact token count. It is 0 before the first API response and
    # after /compact, so fall back to deriving it from the rounded percentage.
    if [ "$in_tok" -gt 0 ] 2>/dev/null; then
        used_k=$(( in_tok / 1000 ))
    else
        used_k=$(( max_ctx * pct / 100 / 1000 ))
    fi
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

    context_info="${bar} ${used_k}k/${max_k}k"
fi

# Rate limit: subscriber-only, and only worth the space once it climbs.
rl_info=""
if [ -n "$rl_pct" ] && [ "$rl_pct" -ge 50 ] 2>/dev/null; then
    rl_info=" | ${RED}rl ${rl_pct}%${RESET}"
fi

# Lines changed this session, once there are any.
lines_info=""
if [ "$lines_add" -gt 0 ] 2>/dev/null || [ "$lines_del" -gt 0 ] 2>/dev/null; then
    lines_info=" | +${lines_add}/-${lines_del}"
fi

printf '%b\n' "${model}${git_info}${pr_info} | ${context_info}${rl_info}${lines_info}"
