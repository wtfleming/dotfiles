#!/bin/bash

# Check if iTerm2 is the frontmost app
frontmost_app=$(osascript -e 'tell application "System Events" to get name of first application process whose frontmost is true' 2>/dev/null)
if [ "$frontmost_app" = "iTerm2" ]; then
  exit 0
fi

if [ -n "$TMUX" ]; then
  # get the tmux session name
  tmux_session=$(tmux display-message -p '#S')
  terminal-notifier -title "Claude" -message "Requesting user input in tmux session: $tmux_session"
else
  terminal-notifier -title "Claude" -message "Requesting user input"
fi
