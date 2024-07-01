#!/bin/bash

SRCDIR="$HOME/.emacs.d/extensions"

# Find all .el files and subdirectories; print after the first line
# Only print, do no act on anything
find "$SRCDIR" -maxdepth 1 -type f -iname '*.el' -o -type d | tail -n +2 |\
    parallel -X "cp -vrft . {}"

# Copy all .el files to the current directory
# cp -v -t . "$SRCDIR"/*.el
