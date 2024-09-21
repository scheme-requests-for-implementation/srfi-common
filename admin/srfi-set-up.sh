#!/bin/bash

set -e

# Do this in several <rsync> steps because we need to put only a
# subset of the files in "srfi.tgz".

if [ "$#" -eq 0 ]; then
  DESTINATION=speechcode.com:/var/www/
elif [ "$#" -eq 1 ]; then
  DESTINATION=$1
else
  echo "Usage: $0 [destination]"
  echo
  echo "The default destination is srfi.schemers.org."
  exit 1
fi

SRFI_ROOT=$(realpath "`srfi common-dir`/..")
STAGING=$(mktemp --directory -t srfi-staging-XXXXX)
STAGING_EMAIL=$STAGING/srfi-email
STAGING_NON_EMAIL=$STAGING/srfi
STAGING_GLOBAL_TGZ=`mktemp`

trap "rm -rf $STAGING/" 0 1 15
mkdir -p $STAGING_EMAIL
mkdir -p $STAGING_NON_EMAIL
rsync \
  --exclude='*~' \
  --exclude='.gitignore' \
  --exclude='.git/' \
  --exclude='.reuse/' \
  --exclude='srfi-email/' \
  --perms \
  --quiet \
  --recursive \
  --safe-links \
  --times \
  $SRFI_ROOT/ \
  $SRFI_ROOT/srfi-common/* \
  $STAGING_NON_EMAIL/
cd $STAGING_EMAIL/
rsync \
  --exclude='*~' \
  --exclude='.gitignore' \
  --exclude='.git/' \
  --exclude='.reuse/' \
  --perms \
  --quiet \
  --recursive \
  --safe-links \
  --times \
  $SRFI_ROOT/srfi-email/ \
  $STAGING_EMAIL/
cd $STAGING_NON_EMAIL/
tar czf $STAGING_GLOBAL_TGZ .
mv $STAGING_GLOBAL_TGZ srfi.tgz
for SRFI in srfi-*;
do
    [ -d "$SRFI" ] || continue
    (tar czf $SRFI.tgz $SRFI/; mv $SRFI.tgz $SRFI/)
done
chmod -R 0755 $STAGING/
for DIR in srfi srfi-email; do
  rsync \
    --checksum \
    --delete \
    --out-format='%n' \
    --perms \
    --recursive \
    --times \
    $STAGING/$DIR/ \
    "$DESTINATION"/$DIR/ \
    | grep --line-buffered -v '/$'
done