#!/bin/sh

mvn -Dscala.version=2.8.0 clean package
#mvn -Dscala.version=2.8.0-SNAPSHOT -Dmaven.test.skip=true clean package
