package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.Compiler
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.HashMap

trait DeclarationIndexes {
    
  self: scala.tools.refactoring.Compiler =>
  
  import global._
  
  class DeclarationIndex extends Function1[Symbol, DefTree] {
  
    private val defs = HashMap[Symbol, DefTree]()
    
    private object defTreeTraverser extends Traverser {
      override def traverse(t: Tree) = {
        t match {
          case t: DefTree => 
            defs += t.symbol → t
          case _ => ()
        }
        super.traverse(t)
      }
    }
    
    def apply(s: Symbol) = {
      defs(s)
    }
    
    def processTree(t: Tree) = defTreeTraverser.traverse(t)
  }
}
