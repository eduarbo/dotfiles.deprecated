#!/bin/bash
# Open a new instance or focus the existing one
open -a "$1"

# Find oldest process id to focus it with kwm
/usr/local/bin/kwmc window -f id $(ps aux | grep -v grep | grep -i "$1" | awk '{print $2;}' | sort -n | head -1)
