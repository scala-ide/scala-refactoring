#!/bin/sh

# To sign the artifacts, add the release-sign-artifacts profile to the command

mvn -Dscala.version=2.10.3-SNAPSHOT -P scala-2.10.x clean package $*
