#!/bin/bash

# Do this in three <rsync> steps because we need to put only a subset
# of the files in "srfi.tgz".

SOURCE=~/srfi/split
DESTINATION=/var/www/srfi
PARENT=`dirname $DESTINATION`
DESTINATION_EMAIL=$PARENT/srfi-email
DESTINATION_NO_EMAIL=$PARENT/srfi-no-email

mkdir -p $DESTINATION_EMAIL
mkdir -p $DESTINATION_NO_EMAIL

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
  $DESTINATION_NO_EMAIL/

TEMPFILE=`mktemp`

(cd $DESTINATION_NO_EMAIL/; tar czf $TEMPFILE --exclude=srfi.tgz .)
mv $TEMPFILE $DESTINATION_EMAIL/srfi.tgz
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
  $DESTINATION_EMAIL/
rsync \
  --checksum \
  --delete \
  --progress \
  --recursive \
  --times \
  /var/www/srfi-no-email/ \
  /var/www/srfi-email/ \
  $DESTINATION/
chmod -R 0755 $DESTINATION/