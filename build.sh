#!/bin/sh

#mvn -Dscala.version=2.9.3-SNAPSHOT  -P scala-2.9.x  clean package $*
#mvn -Dscala.version=2.10.0-SNAPSHOT -P scala-2.10.x clean package $*
#mvn -Dscala.version=2.10.1-SNAPSHOT -P scala-2.10.1 clean package $*
mvn -Dscala.version=2.11.0-SNAPSHOT -P scala-2.11.x clean package $*
