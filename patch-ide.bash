#!/bin/bash


SCRIPT_NAME="./$(basename "$0")"

showHelp() {
  echo "Patches ScalaIDE with the latest local build"
  echo "--------------------------------------------"
  echo ""
  echo "The script is controlled by the following environment variables:"
  echo "  SCALA_IDE_HOME (mandatory):"
  echo "    Path to you local ScalaIDE installation"
  echo ""
  echo "Examples: "
  echo "  SCALA_IDE_HOME=\"/path/to/scala-ide\" $SCRIPT_NAME"
  echo "  SCALA_IDE_HOME=\"/path/to/scala-ide\" KEEP_REFACTORING_LIBRARY_BACKUP=true $SCRIPT_NAME"
  echo ""
  echo "Best practice:"
  echo "  If you use the script regularly, it is recommended to export"
  echo "  an appropriate value for SCALA_IDE_HOME via your bashrc, so that you"
  echo "  don't have to specify this setting repeatedly."
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

TARGET_FOLDER="./target/scala-2.11/"

shopt -s nullglob
_newRefactoringJars=("$TARGET_FOLDER"*SNAPSHOT.jar)
NEW_REFACTORING_JAR="${_newRefactoringJars[0]}"

if [[ ! -f "$NEW_REFACTORING_JAR" || ! -r "$NEW_REFACTORING_JAR" ]]; then
  echoErr "Cannot find a build of the library in $TARGET_FOLDER"
  exit 1
fi

TSTAMP="$(date +%Y-%m-%dT%H-%M-%S)"

_oldRefactoringJar=($SCALA_IDE_PLUGINS_DIR/org.scala-refactoring.library*.jar)
if [[ ${#_oldRefactoringJar[@]} == 0 ]]; then
  echoErr "Cannot find the refactoring library in $SCALA_IDE_PLUGINS_DIR"
  exit 1
elif [[ ${#_oldRefactoringJar[@]} -gt 1 ]]; then
  echoErr "Multiple copies of the refactoring library found in $SCALA_IDE_PLUGINS_DIR:"
  for jarFile in "${_oldRefactoringJar[@]}"; do 
    echoErr "  $jarFile"
  done
  exit 1
fi

OLD_REFACTORING_JAR="${_oldRefactoringJar[0]}"
BACKUP_REFACTORING_JAR="$OLD_REFACTORING_JAR.$TSTAMP.bak"
cp "$OLD_REFACTORING_JAR" "$BACKUP_REFACTORING_JAR"
cp -i "$NEW_REFACTORING_JAR" "$OLD_REFACTORING_JAR"
