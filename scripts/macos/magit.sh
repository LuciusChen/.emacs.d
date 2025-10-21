#!/usr/bin/env bash

set -o errexit

# Get the root of the git repository
git_root=$(git rev-parse --show-toplevel)

# Open magit-status in Emacs
emacsclient -e "(magit-status \"${git_root}\")" > /dev/null

# Bring Emacs frame to the foreground (macOS specific)
osascript << EOF
tell application "System Events"
        tell application process "Emacs"
                set frontmost to true
        end tell
end tell
EOF

# Pass any additional arguments to emacsclient
# https://dolzhenko.me/blog/2025/03/launching-magit-from-intellij-idea/
/opt/homebrew/bin/emacsclient "$@"
