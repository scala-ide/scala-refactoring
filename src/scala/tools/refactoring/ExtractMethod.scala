package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class ExtractMethod(override val global: Global) extends Refactoring(global) {
  
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
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet] = {
    
    import prepared._
    import params._
    
    indexFile(selection.file)

    val parameters = inboundLocalDependencies(selection, selectedMethod.symbol)
    
    val returns = outboundLocalDependencies(selection, selectedMethod.symbol)
     
    val newDef = mkDefDef(NoMods, methodName, parameters :: Nil, selection.selectedTopLevelTrees ::: (if(returns.isEmpty) Nil else mkReturn(returns) :: Nil))
    
    val call = mkCallDefDef(NoMods, methodName, parameters :: Nil, returns)
    
    val changes = new ChangeCollector {
      transform(selection.file) {
        case d: DefDef if d == selectedMethod /*ensure that we don't replace from the new method :) */ => {
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
        case tpl @ Template(_, _, body) if body exists (_ == selectedMethod) => {
          tpl.copy(body = replace(body, selectedMethod :: Nil, selectedMethod :: newDef :: Nil)) setPos tpl.pos
        }
      }
    }
    
    Right(refactor(selection.file, changes))
  }
}
