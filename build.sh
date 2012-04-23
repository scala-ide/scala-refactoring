#!/bin/sh

mvn -Dscala.version=2.9.3-SNAPSHOT -P scala-2.9.x clean package $*
mvn -Dscala.version=2.10.0-SNAPSHOT -P scala-trunk clean package $*
