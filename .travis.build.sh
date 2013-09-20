#!/bin/bash

MINOR_VERSION=`echo "$TRAVIS_SCALA_VERSION" | sed 's/^\([0-9]*\)\.\([0-9]*\)\.[^\.]*$/\1.\2/g'`

echo "Build for scala $MINOR_VERSION"

eval "./build-$MINOR_VERSION.sh"