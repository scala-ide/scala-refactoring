/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.reflect.internal.util.SourceFile

trait AbstractPrinter extends CommonPrintUtils {

  this: common.Tracing with common.PimpedTrees with Indentations with common.CompilerAccess with common.CompilerApiExtensions with Formatting =>

  import global._

  /**
   * PrintingContext is passed around with all the print methods and contains
   * the context or environment for the current printing.
   */
  case class PrintingContext(ind: Indentation, changeSet: ChangeSet, parent: Tree, file: Option[SourceFile]) {
    private lazy val lexical = file map (new LexicalStructure(_))

    def tokensBetween(start: Int, end: Int): Seq[Token] =
      lexical.map(_.tokensBetween(start, end)).getOrElse(Seq())

    lazy val newline: String = {
      if(file.exists(_.content.containsSlice("\r\n")))
        "\r\n"
      else
        "\n"
    }
  }

  trait ChangeSet {
    def hasChanged(t: Tree): Boolean
  }

  def print(t: Tree, ctx: PrintingContext): Fragment

}
