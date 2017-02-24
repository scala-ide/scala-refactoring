/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring.sourcegen

import scala.util.Properties

/**
 * Holds default formatting preferences.
 */
trait Formatting {

  /**
   * The characters that are used to indent changed code.
   */
  def defaultIndentationStep = "  "

  /**
   * The characters that surround an import with multiple
   * import selectors inside the braces:
   *
   *   import a.{*name*}
   */
  def spacingAroundMultipleImports = ""

  /**
   * If set to `true` printer of import should drop `scala.` prefix:
   *
   * `import scala.util.Try` should be printed as
   *
   * `import util.Try`
   */
  def dropScalaPackage = false

  /** Used when new line is added to source file and EOL is needed. */
  def lineDelimiter = Properties.lineSeparator
}
