package scala.tools.refactoring

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global

class OrganizeImports (override val global: Global) extends Refactoring(global) {
  
  import global._
  
  trait PreparationResult {
    def file: AbstractFile
  }
  
  class RefactoringParameters
  
  def prepare(f: AbstractFile, from: Int, to: Int): Either[PreparationError, PreparationResult] = {
    Right(new PreparationResult {
      val file = f
    })
  }
    
  def perform(prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, ChangeSet] = {
    
    def sortImports(imports: List[Tree]) = imports.sortWith({
      case (t1: Import, t2: Import) => t1.expr.toString < t2.expr.toString
    }: (Tree, Tree) => Boolean)
    
    def collapseImports(imports: List[Tree]) = {

      imports.foldRight(List[Import]()) { 
        case (imp: Import, Nil) => imp :: Nil
        case (imp: Import, x :: xs) if imp.expr.toString == x.expr.toString => x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
        case (imp: Import, xs) => imp :: xs
      }
      
    }
    
    var newTree = transform(prepared.file) {
      case p @ PackageDef(_, stats) => {
        
        val sorted = stats partition {
          case _: Import => true
          case _ => false
        } match {
          case (imports, others) => collapseImports(sortImports(imports)) ::: others
        }
        
        p copy (stats = sorted)
      }
    }
    
    Right(refactor(prepared.file, newTree))
  }
}
