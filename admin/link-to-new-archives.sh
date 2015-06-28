#!/bin/bash

function insert_notice {
    FILE=$1
    TEMP=`mktemp`

    chmod go+r $TEMP
    awk -v "notice=$NOTICE" '/<h1>/{print;print notice;next}1' $FILE>$TEMP
    mv $TEMP ./$FILE
}

for DIR in `find . -maxdepth 1 -type d -name 'srfi-*'`
do
    N=`echo $DIR|cut -d '-' -f 2`
    NOTICE="<p style=\"max-width: 30em;\">This page is part of the web mail archives of <a href=\"http://srfi.schemers.org/srfi-$N\">SRFI $N</a> from before July 7th, 2015.  The new archives for SRFI $N are <a href=\"http://srfi-email.schemers.org/srfi-$N/\">here</a>.  Eventually, the entire history will be moved there, including any new messages.</p>"

    if [ -d "$DIR/mail-archive" ]; then
	pushd $DIR/mail-archive>/dev/null
	for MESSAGE in `find . -name maillist.html -o -name threads.html -o -name 'msg*.html'`
	do
	    insert_notice $MESSAGE
	done
	popd>/dev/null
    fi
done