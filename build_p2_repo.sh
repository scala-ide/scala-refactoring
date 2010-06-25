#!/bin/sh

THIS=$(readlink -f $0)
BUNDLE_ROOT="`dirname $THIS`/scala-refactoring-updatesite/target/site"

eclipse -consolelog -nosplash -application org.eclipse.equinox.p2.publisher.FeaturesAndBundlesPublisher -metadataRepository file:/$BUNDLE_ROOT/repo -artifactRepository file:/$BUNDLE_ROOT=/repo -source $BUNDLE_ROOT -compress -publishArtifacts
