#!/bin/sh

# To sign the artifacts, add the release-sign-artifacts profile to the command

mvn -P scala-2.11.x clean package $*
