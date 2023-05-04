#!/bin/sh

if [ -f main.sld ]; then
    printf "%s\n" "Making chibi-srfi.img"
    chibi-scheme -I .. -m srfi-tools.main -d chibi-srfi.img
    printf "%s\n" "Done. The chibi-scheme wrapper should run fast now."
else
    printf "%s\n" "Couldn't find main.sld." "This script should be run from the srfi-common/srfi-tools directory"
    exit 1
fi
