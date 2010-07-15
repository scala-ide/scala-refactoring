/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.TreeAnalysis

abstract class Rename extends MultiStageRefactoring with TreeAnalysis with analysis.Indexes with TreeFactory {
    
  import global._
      
  case class PreparationResult(selectedTree: SymTree, hasLocalScope: Boolean)
  
  type RefactoringParameters = String
  
  def prepare(s: Selection) = {
    s.selectedSymbolTree match {
      case Some(t) =>
        Right(PreparationResult(t, t.symbol.isPrivate || t.symbol.isLocal))
      case None => Left(PreparationError("no symbol selected found"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, newName: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    trace("Selected tree is %s", prepared.selectedTree)
    
    val occurences = index.occurences(prepared.selectedTree.symbol) 
    
    occurences foreach (s => trace("Symbol is referenced at %s (%s:%s)", s, s.pos.source.file.name, s.pos.line))
    
    val isInTheIndex = filter {
      case t: Tree => occurences contains t 
    }
    
    val renameTree = transform {
      case t: ImportSelectorTree => 
        mkRenamedImportTree(t, newName)
      case s: SymTree => 
        mkRenamedSymTree(s, newName)
      case t: TypeTree => 
        mkRenamedTypeTree(t, newName, prepared.selectedTree.symbol)
    }
    
    val rename = topdown(isInTheIndex &> renameTree |> id)
    
    val renamedTrees = occurences flatMap (rename(_))
    
    Right(refactor(renamedTrees))
  }
}
