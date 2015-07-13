#!/bin/bash

set -e

# I use a modified version of Eml2mbox that replaces the initial From
# line rather than adding a new one above the existing one.
EML2MBOX=/Users/arthur/git-clones/eml2mbox/eml2mbox.rb
OUTPUT=$d
SRFI=(`find -E . -maxdepth 1 -type d -regex '.*/srfi-[0-9]+'|awk -F './srfi-' '{print $2'}`)

cd ~/srfi/split/
mkdir -p $OUTPUT/mbox/
for I in ${SRFI[@]} discuss
do
    T=`mktemp -d $OUTPUT/srfi-$I-temp`

    echo $I
    pushd ~/srfi/srfi-schemers-org/srfi/srfi-$I/archive/
    for F in `find latest/ -type f`
    do
	cp $F $T/original-`basename $F`.eml
    done
    for F in `find latest/ -type f`
    do
	cp $F $T/latest-`basename $F`.eml
    done
    cd $T/
    $EML2MBOX . $OUTPUT/mbox/srfi-$I.mbox
    rm -rf $T/
    popd
done