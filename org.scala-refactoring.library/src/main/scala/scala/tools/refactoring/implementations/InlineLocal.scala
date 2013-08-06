/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.Indexes

abstract class InlineLocal extends MultiStageRefactoring with TreeFactory with Indexes with common.InteractiveScalaCompiler  {

  import global._

  type PreparationResult = ValDef

  class RefactoringParameters

  def prepare(s: Selection) = {

    val selectedValue = s.findSelectedOfType[RefTree] match {
      case Some(t) =>
        index.declaration(t.symbol) match {
          case Some(v: ValDef) => Some(v)
          case _ => None
        }
      case None => s.findSelectedOfType[ValDef]
    }

    selectedValue match {
      case Some(t) if (t.symbol.isPrivate || t.symbol.isLocal)
        && !t.symbol.isMutable && !t.symbol.isParameter
        && t.symbol.enclMethod != NoSymbol => Right(t)
      case Some(t) =>
        Left(PreparationError("The selected value cannot be inlined."))
      case _ =>
        Left(PreparationError("No local value selected."))
    }
  }

  def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    trace("Selected: %s", selectedValue)

    val removeSelectedValue = {

      def replaceSelectedValue(ts: List[Tree]) = {
        ts replaceSequence (List(selectedValue), Nil)
      }

      transform {
        case tpl @ Template(_, _, stats) if stats contains selectedValue =>
          tpl.copy(body = replaceSelectedValue(stats)) replaces tpl
        case block @ BlockExtractor(stats) if stats contains selectedValue =>
          mkBlock(replaceSelectedValue(stats)) replaces block
      }
    }

    val replaceReferenceWithRhs = {

      val references = index references selectedValue.symbol

      val replacement = selectedValue.rhs match {
        // inlining `list.filter _` should not include the `_`
        case Function(vparams, Apply(fun, args)) if vparams forall (_.symbol.isSynthetic) => fun
        case t => t
      }

      trace("Value is referenced on lines: %s", references map (_.pos.lineContent) mkString "\n  ")

      transform {
        case t if references contains t => replacement
      }
    }

    Right(transformFile(selection.file, topdown(matchingChildren(removeSelectedValue &> topdown(matchingChildren(replaceReferenceWithRhs))))))
  }
}
