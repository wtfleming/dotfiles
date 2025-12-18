#!/bin/bash

if [ -n "$TMUX" ]; then
  # get the tmux session name
  tmux_session=$(tmux display-message -p '#S')
  terminal-notifier -title "Claude" -message "Requesting user input in tmux session: $tmux_session"
else
  terminal-notifier -title "Claude" -message "Requesting user input"
fi
