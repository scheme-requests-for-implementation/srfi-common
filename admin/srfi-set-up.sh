#!/bin/bash

SOURCE=~/srfi/split
TEMP=`mktemp --directory`
DESTINATION=/var/www/srfi
cd $TEMP/
for DIR in common email
  do
  echo $DIR
  ((cd $SOURCE/srfi-$DIR && git archive --format=tgz HEAD)|tar xzf -)
done
$SOURCE/srfi-common/admin/link-to-new-archives.sh
for I in $(seq 0 121)
  do
  echo srfi-$I
  (cd $SOURCE/srfi-$I && git archive --format=tgz HEAD)|(cd $TEMP/srfi-$I; tar xzf -)
done
cp -p $TEMP/README.html $TEMP/index.html

chmod -R 0755 $TEMP
mv --force $TEMP $DESTINATION