package scala.tools.refactoring

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class RenameLocal(override val global: Global) extends Refactoring(global) {
  
  import global._
  
  abstract class PreparationResult {
    def selection: Selection
    def file: AbstractFile
    def selectedLocal: SymTree
  }
  
  abstract class RefactoringParameters {
    def newName: String
  }
  
  def prepare(f: AbstractFile, from: Int, to: Int) = {
    val s = new Selection(f, from, to)
    s.selectedSymbolTree match {
      case Some(t) =>
        Right(new PreparationResult {
          val selection = s
          val file = f
          val selectedLocal = t
        })
      case None => Left(new PreparationError("no symbol selected found"))
    }
  }
    
  def perform(prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet] = {
    
    import prepared._
    
    indexFile(file)
    
    def rename(t: SymTree, name: String) = t match {
      case i: Ident  => i.copy(name = name)
      case v: ValDef => v.copy(name = name)
      case d: DefDef => d.copy(name = name)
      case b: Bind   => b.copy(name = name)
      case s: Select => s.copy(name = name)
      case t => throw new Exception("Found "+ t.getClass.getName)
    }
    
    trace("Selected tree is %s", selectedLocal)

    val changes = new ChangeCollector {
      transform(file) {
        case s: SymTree if s.symbol == selectedLocal.symbol => rename(s, params.newName)
      }
    }.changedTrees
    
    Right(refactor(file, changes._1, changes._2))
  }
}
