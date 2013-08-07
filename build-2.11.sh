#!/bin/sh

# To sign the artifacts, add the release-sign-artifacts profile to the command

mvn -Dscala.version=2.11.0-SNAPSHOT -P scala-2.11.x clean package $*
