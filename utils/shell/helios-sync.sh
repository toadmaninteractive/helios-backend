#!/bin/bash

# $1 :: input directory (should include game GUID and build-rev like: /tmp/ci/sfl-112 having subdirectory /sfl/112)
# $2 :: output directory
CMD="rsync -r $1/ $2"
RESULT=`$CMD`
echo "ok"
