#!/bin/bash

DESTINATION=/var/www/srfi
PARENT=`dirname $DESTINATION`
D=`basename $DESTINATION`

cd $PARENT

umount $DESTINATION
mount -t overlay -olowerdir=srfi-upper:srfi-lower $D $D