/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring.sourcegen

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
}