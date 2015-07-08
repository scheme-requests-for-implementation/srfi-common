#!/bin/bash

# Use an overlay because we run <rsync> twice to copy the contents of
# all the repositories in place, but we need to use <--delete> to
# delete obsolete files.  Without an overlay, that would cause the
# first <rsync>'s files to be deleted.  I'd prefer a less complex way
# of doing this that was still fast.  Using an overlay has the side
# benefit of making constructing "srfi.tgz" trivial without having to
# jump through hoops for exclusions.

# Note that, even in a writeable overlay (i.e. one where a <workdir>
# is specified), writes to the underlying lower and upper filesystems
# are not supported while the mount is active.  "If the underlying
# filesystem is changed, the behavior of the overlay is undefined,
# though it will not result in a crash or deadlock."  (See
# <https://goo.gl/fXc9Zq>.)  So I do it anyway, then unmount and
# re-mount.

# TODO: Rewrite this in Scheme without overlays, combining the <rsync>
# steps into one.

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
    --exclude='.gitignore' \
    --exclude='.git/' \
    --exclude='*~' \
    --exclude='admin/' \
    --progress \
    --recursive \
    --safe-links \
    --times \
    $SOURCE/srfi-common/* \
    $SOURCE/srfi-email/* \
    $DESTINATION_UPPER/
cp --force -p $DESTINATION_UPPER/README.html $DESTINATION_UPPER/index.html
chmod -R 0755 $DESTINATION_UPPER
chmod -R 0755 $DESTINATION_LOWER
echo "Don't forget to unmount and re-mount $DESTINATION."
echo "Use <sudo srfi-remount.sh> after checking its contents." 