/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait AbstractPrinter extends SourceCodeHelpers with CommonPrintUtils {

  this: common.Tracing with common.PimpedTrees with Indentations with common.CompilerAccess =>

  import global._

  /**
   * PrintingContext is passed around with all the print methods and contains 
   * the context or environment for the current printing.
   */
  case class PrintingContext(ind: Indentation, changeSet: ChangeSet, parent: Tree)

  trait ChangeSet {
    def hasChanged(t: Tree): Boolean
  }

  def print(t: Tree, ctx: PrintingContext): Fragment

}