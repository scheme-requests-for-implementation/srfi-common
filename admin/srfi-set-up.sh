#!/bin/bash

# Do this in three <rsync> steps because we need to put only a subset
# of the files in "srfi.tgz".

SOURCE=~/srfi/split
DESTINATION=/var/www/srfi
PARENT=`dirname $DESTINATION`
DESTINATION_UPPER=$PARENT/srfi-upper
DESTINATION_LOWER=$PARENT/srfi-lower

mkdir -p $DESTINATION_UPPER
mkdir -p $DESTINATION_LOWER

rsync \
    --checksum \
    --delete \
    --exclude='*~' \
    --exclude='.gitignore' \
    --exclude='.git/' \
    --exclude='srfi-common/' \
    --exclude='srfi-email/' \
    --progress \
    --recursive \
    --safe-links \
    --times \
    $SOURCE/ \
    $DESTINATION_LOWER/

TEMPFILE=`mktemp`

(cd $DESTINATION_LOWER/; tar czf $TEMPFILE --exclude=srfi.tgz .)
mv $TEMPFILE $DESTINATION_UPPER/srfi.tgz
rsync \
    --checksum \
    --delete \
    --exclude='*~' \
    --exclude='.gitignore' \
    --exclude='.git/' \
    --exclude='admin/' \
    --progress \
    --recursive \
    --safe-links \
    --times \
    $SOURCE/srfi-common/* \
    $SOURCE/srfi-email/* \
    $DESTINATION_UPPER/
cp --force -p $DESTINATION_UPPER/README.html $DESTINATION_UPPER/index.html
rsync \
    --checksum \
    --delete \
    --progress \
    --recursive \
    --times \
    /var/www/srfi-lower/ \
    /var/www/srfi-upper/ \
    $DESTINATION/
chmod -R 0755 $DESTINATION/
