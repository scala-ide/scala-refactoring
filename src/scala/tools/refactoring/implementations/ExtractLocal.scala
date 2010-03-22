package scala.tools.refactoring.implementations

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.analysis.FullIndexes

abstract class ExtractLocal(override val global: Global) extends MultiStageRefactoring(global) {
  
  import global._
  
  abstract class PreparationResult {
    def selectedExpression: Tree
  }
  
  abstract class RefactoringParameters {
    def name: String
  }
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[TermTree] match {
      case Some(term) =>
        Right(new PreparationResult {
          val selectedExpression = term
        })
      case None => Left(new PreparationError("no term selected"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, TreeModifications] = {
    
    import prepared._
    import params._
    
    implicit def replacesTree(t1: Tree) = new {
      def replaces(t2: Tree) = t1 setPos t2.pos
    }
    
    trace("Selected: %s", selectedExpression)
    
    val newVal = mkValDef(name, selectedExpression)
    val valRef = Ident(name)
    
    val changes = new ModificationCollector {
      transform(selection.file) {
        case block: Block if block.pos.includes(selectedExpression.pos) =>
        
          val stats = block map (transform(_) {
            case t: TermTree if t == selectedExpression =>
              valRef replaces selectedExpression
          })
        
          mkBlock(newVal :: stats) replaces block
      }
    }
    
    Right(changes)
  }
}
