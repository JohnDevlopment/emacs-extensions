#!/bin/bash

SRCDIR=~/.emacs.d/extensions

# Find all .el files and subdirectories; print after the first line
# Only print, do no act on anything
# find $SRCDIR -maxdepth 2 -type f -name '*.el' | parallel -X "cp -vrft . {}"

cp -vft ./ $SRCDIR/*{.el,.el.gz}
cp -vfrt ./ $SRCDIR/packages/
rm packages/*.elc
