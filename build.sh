#!/bin/sh

#mvn -Dscala.version=2.8.0 clean package -Dmaven.test.skip=true $*
mvn -Dscala.version=2.8.1 clean package $*
#mvn -Dscala.version=2.8.1-SNAPSHOT clean package
#mvn -Dscala.version=2.9.0-SNAPSHOT -Dmaven.test.skip=true clean package
