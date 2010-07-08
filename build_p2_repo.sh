#!/bin/sh

THIS=$(readlink -f $0)
BUNDLE_ROOT="`dirname $THIS`/org.scala-refactoring.update-site/target/site"

eclipse -consolelog -nosplash -application org.eclipse.equinox.p2.publisher.FeaturesAndBundlesPublisher -metadataRepository file:/$BUNDLE_ROOT/repo -artifactRepository file:/$BUNDLE_ROOT/repo -source $BUNDLE_ROOT -compress -publishArtifacts
