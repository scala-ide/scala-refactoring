package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change

class RenameLocal(override val global: Global) extends Refactoring(global) {
  
  import global._
  
  abstract class PreparationResult {
    def selectedLocal: SymTree
  }
  
  abstract class RefactoringParameters {
    def newName: String
  }
  
  def prepare(s: Selection) = {
    s.selectedSymbolTree match {
      case Some(t) =>
        Right(new PreparationResult {
          val selectedLocal = t
        })
      case None => Left(PreparationError("no symbol selected found"))
    }
  }
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
    val index = new Index {
      processTree(selection.file)
    }
    
    trace("Selected tree is %s", prepared.selectedLocal)

    val changes = new ChangeCollector {
      transform(selection.file) {
        case s: SymTree if s.symbol == prepared.selectedLocal.symbol => mkRenamedSymTree(s, params.newName)
      }
    }
    
    Right(refactor(selection.file, changes))
  }
}
