/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.Indexes

abstract class InlineLocal extends MultiStageRefactoring with TreeFactory with Indexes {
  
  import global._
  
  type PreparationResult = ValDef
  
  class RefactoringParameters
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ValDef] match {
      case Some(t) if (t.symbol.isPrivate || t.symbol.isLocal) && !t.symbol.isMutable => Right(t)
      case _ => Left(PreparationError("No local value selected."))
    }
  }
    
  def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
            
    trace("Selected: %s", selectedValue)
    
    val references = index references selectedValue.symbol
    
    val replacement = selectedValue.rhs match {
      // inlining `list.filter _` should not include the `_`
      case Function(vparams, Apply(fun, args)) if vparams forall (_.symbol.isSynthetic) => fun
      case t => t
    }
    
    trace("Value is referenced on lines: %s", references map (_.pos.lineContent) mkString "\n  ")
    
    def replateSelectedValueInBlock(ts: List[Tree]) = {
      ts replaceSequence (List(selectedValue), Nil)
    }
    
    val removeSelectedValue = transform {
      case tpl @ Template(_, _, stats) if stats contains selectedValue =>
        tpl.copy(body = replateSelectedValueInBlock(stats)) replaces tpl
      case block @ BlockExtractor(stats) if stats contains selectedValue =>
        mkBlock(replateSelectedValueInBlock(stats)) replaces block
    }
    
    val replaceReferenceWithRhs = transform {
      case t if references contains t => replacement
    }
    
    val inlined = topdown(matchingChildren(removeSelectedValue &> topdown(matchingChildren(replaceReferenceWithRhs)))) apply selection.root
  
    Right(refactor(inlined.toList))
  }
}
