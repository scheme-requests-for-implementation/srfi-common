#!/bin/bash

SOURCE=~/srfi/split
DESTINATION=/var/www/srfi
PARENT=`dirname $DESTINATION`
TEMPDIR_DEL=`mktemp --directory --tmpdir=$PARENT`
TEMPDIR_NEW=`mktemp --directory --tmpdir=$PARENT`

cd $SOURCE

SRFI=(`find . -maxdepth 1 -type d -regex './srfi-[0-9]+'|awk -F './srfi-' '{print $2'}`)

cd $TEMPDIR_NEW/
for DIR in common email
do
    echo $DIR
    ((cd $SOURCE/srfi-$DIR && git archive --format=tgz HEAD)|tar xzf -)
done
$SOURCE/srfi-common/admin/link-to-new-archives.sh
for I in ${SRFI[@]}
do
    echo srfi-$I
    (cd $SOURCE/srfi-$I && git archive --format=tgz HEAD)|(cd $TEMPDIR_NEW/srfi-$I; tar xzf -)
done
cp -p $TEMPDIR_NEW/README.html $TEMPDIR_NEW/index.html
chmod -R 0755 $TEMPDIR_NEW
mv -T $DESTINATION $TEMPDIR_DEL	  # race condition
mv $TEMPDIR_NEW $DESTINATION
rm -rf $TEMPDIR_DEL