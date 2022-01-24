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

# Delete ".tgz" files so rsync won't report that it's deleting them.
pushd $DESTINATION_NO_EMAIL/
for SRFI in srfi-*; do rm -f $SRFI/$SRFI.tgz; done
popd

rsync \
  --checksum \
  --delete \
  --exclude='*~' \
  --exclude='.gitignore' \
  --exclude='.git/' \
  --exclude='srfi-email/' \
  --recursive \
  --safe-links \
  --times \
  $SOURCE/ \
  $DESTINATION_NO_EMAIL/

TEMPFILE=`mktemp`

pushd $DESTINATION_NO_EMAIL/
tar czf $TEMPFILE --exclude=srfi.tgz .
mv $TEMPFILE $DESTINATION_EMAIL/srfi.tgz
for SRFI in srfi-*;
do
    (tar czf $SRFI.tgz $SRFI/; mv $SRFI.tgz $SRFI)
done
popd
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
  --out-format='%n' \
  --recursive \
  --times \
  /var/www/srfi-no-email/ \
  /var/www/srfi-email/ \
  $DESTINATION/ \
  | grep --line-buffered -v '/$'
chmod -R 0755 $DESTINATION/