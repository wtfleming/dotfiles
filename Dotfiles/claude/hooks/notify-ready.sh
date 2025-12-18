#!/bin/bash

if [ -n "$TMUX" ]; then
  # get the tmux session name
  tmux_session=$(tmux display-message -p '#S')
  terminal-notifier -title "Claude" -message "Waiting in tmux session: $tmux_session"
else
  terminal-notifier -title "Claude" -message "Waiting for input"
fi
