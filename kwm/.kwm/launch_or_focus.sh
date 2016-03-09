#!/bin/bash
# Open a new instance or focus the existing one
open -a "$1"

# Find oldest process id to focus it with kwm
win_id=$(/Users/eduarbo/bin/kwmc read windows | grep -i "$1" | head -1 | awk '{print $1;}')
# remove trailing comma
win_id=${win_id%?}
/Users/eduarbo/bin/kwmc window -f id $win_id
