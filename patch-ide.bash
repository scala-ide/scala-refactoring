#!/bin/bash


SCRIPT_NAME="./$(basename "$0")"

showHelp() {
  echo "Patches ScalaIDE with the latest local build"
  echo "--------------------------------------------"
  echo ""
  echo "The script is controlled by the following environment variables:"
  echo "  SCALA_IDE_HOME (mandatory):"
  echo "    Path to you local ScalaIDE installation"
  echo "  KEEP_REFACTORING_LIBRARY_BACKUP (defaults to true):"
  echo "    Tells the script weather to keep a backup of the old library"
  echo ""
  echo "Examples: " 
  echo "  SCALA_IDE_HOME=\"/path/to/scala-ide\" $SCRIPT_NAME"
  echo "  SCALA_IDE_HOME=\"/path/to/scala-ide\" KEEP_REFACTORING_LIBRARY_BACKUP=true $SCRIPT_NAME"
  echo ""
  echo "Best practice:"
  echo "  If you use the script regularly, it is recommended to to export"
  echo "  appropriate values for SCALA_IDE_HOME and"
  echo "  KEEP_REFACTORING_LIBRARY_BACKUP via your bashrc, so that you"
  echo "  don't have to specify these values repeatedly."
  echo ""
  echo "Warning:"
  echo "  Note that patching the IDE like this only works as long as"
  echo "  binary compatibility is maintained. Watch out for"
  echo "  AbstractMethodErrors and the like."
}

showHelpAndDie() {
  showHelp
  exit 1
}

echoErr() {
  cat <<< "$@" 1>&2
}

KEEP_REFACTORING_LIBRARY_BACKUP=${KEEP_REFACTORING_LIBRARY_BACKUP:-true}

if [[ -z "$SCALA_IDE_HOME" ]]; then
  showHelpAndDie
fi

SCALA_IDE_PLUGINS_DIR="$SCALA_IDE_HOME/plugins"

if [[ ! -d "$SCALA_IDE_PLUGINS_DIR" || ! -w "$SCALA_IDE_PLUGINS_DIR" ]]; then
  echoErr "Invalid SCALA_IDE_HOME: $SCALA_IDE_PLUGINS_DIR is not a writable directory"
  exit 1
fi

TARGET_FOLDER="./org.scala-refactoring.library/target/"

_newRefactoringJars=("$TARGET_FOLDER"*SNAPSHOT.jar)
NEW_REFACTORING_JAR="${_newRefactoringJars[0]}"

if [[ ! -f "$NEW_REFACTORING_JAR" || ! -r "$NEW_REFACTORING_JAR" ]]; then
  echoErr "Cannot find a build of the library in $TARGET_FOLDER"
  exit 1
fi

TSTAMP="$(date +%Y-%m-%dT%H-%M-%S)"

for oldRefactoringJar in "$SCALA_IDE_PLUGINS_DIR/"org.scala-refactoring.library_*.jar; do
  if [[ "$KEEP_REFACTORING_LIBRARY_BACKUP" == "true" ]]; then
    backupRefactoringJar="$oldRefactoringJar.$TSTAMP.bak"
    mv "$oldRefactoringJar" "$backupRefactoringJar"
  else
    rm "$oldRefactoringJar"
  fi
done

REFACTORING_JAR_DEST_NAME="org.scala-refactoring.library_localbuild-$TSTAMP-SNAPSHOT.jar"
REFACTORING_JAR_DESTINATION="$SCALA_IDE_PLUGINS_DIR/$REFACTORING_JAR_DEST_NAME"

cp "$NEW_REFACTORING_JAR" "$REFACTORING_JAR_DESTINATION"
