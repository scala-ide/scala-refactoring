#!/bin/sh

#mvn -Dscala.version=2.8.0 clean package $*
#mvn -Dscala.version=2.8.1 clean package $*
#mvn -Dscala.version=2.8.2-SNAPSHOT clean package $*
#mvn -Dscala.version=2.9.0 clean package $*
#mvn -Dscala.version=2.9.0-1 clean package $*
#mvn -Dscala.version=2.9.1-SNAPSHOT clean package $*
mvn -Dscala.version=2.10.0-SNAPSHOT clean package $*
