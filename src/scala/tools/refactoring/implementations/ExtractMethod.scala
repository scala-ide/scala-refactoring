package scala.tools.refactoring.implementations

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.analysis.FullIndexes

abstract class ExtractMethod(override val global: Global) extends MultiStageRefactoring(global) {
  
  import global._
  
  abstract class PreparationResult {
    def selectedMethod: Tree
  }
  
  abstract class RefactoringParameters {
    def methodName: String
  }
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[DefDef] match {
      case Some(defdef) =>
        Right(new PreparationResult {
          val selectedMethod = defdef
        })
      case None => Left(new PreparationError("no enclosing defdef found"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, TreeModifications] = {
    
    import prepared._
    import params._

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol, index)
    
    val returns = outboundLocalDependencies(selection, selectedMethod.symbol, index)
     
    val newDef = mkDefDef(NoMods, methodName, parameters :: Nil, selection.selectedTopLevelTrees ::: (if(returns.isEmpty) Nil else mkReturn(returns) :: Nil))
    
    val call = mkCallDefDef(NoMods, methodName, parameters :: Nil, returns)
    
    val changes = new ModificationCollector {
      transform(selection.file) {
        case tpl @ Template(_, _, body) if body exists (_ == selectedMethod) => {
          
          val refactoredMethod = transform(selectedMethod) {
            case d: DefDef if d == selectedMethod => {
              if(selection.selectedTopLevelTrees.size > 1) {
                transform(d) {
                  case block: Block => {
                    mkBlock(replace(block, selection.selectedTopLevelTrees, call :: Nil)) setPos block.pos
                  }
                }
              } else {
                transform(d) {
                  case t: Tree if t == selection.selectedTopLevelTrees.head => call setPos t.pos
                }
              }
            } 
          }
          
          tpl.copy(body = replace(body, selectedMethod :: Nil, refactoredMethod :: newDef :: Nil)) setPos tpl.pos
        }
      }
    }
    
    Right(changes)
  }
}
