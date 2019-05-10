tmux has-session -t lorem-dev

if [ "$?" -eq 1 ]; then
    tmux new-session -s lorem-dev -n editor -d

    # C-m sends a Carriage Return sequence
    tmux send-keys -t lorem-dev 'cd ~/src/lorem-api' C-m

    tmux split-window -v -t lorem-dev

    tmux select-layout -t lorem-dev main-horizontal

    # Send a command to session lorem-dev, window 1, pane 2
    tmux send-keys -t lorem-dev:1.2 'cd ~/src/lorem-api' C-m

    tmux new-window -n console -t lorem-dev
    tmux send-keys -t lorem-dev:2 'cd ~/src/lorem-api' C-m

    # When we start our session we want our first window to be displayed
    tmux select-window -t lorem-dev:1
fi

tmux attach -t lorem-dev