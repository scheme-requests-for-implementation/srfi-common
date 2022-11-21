#!/bin/sh

FILES_PATTERN='\.(scm|sld)(\..+)?$'
FORBIDDEN='FIXME'
git diff --name-only --staged \
  | grep --extended-regexp $FILES_PATTERN \
  | xargs \
    grep \
    --fixed-strings \
    --line-number \
    --with-filename \
    $FORBIDDEN \
  && echo "Commit rejected because \"$FORBIDDEN\" found." \
  && exit 1
exit 0