package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.Compiler
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.{HashMap, ListBuffer}

trait DeclarationIndexes {
    
  self: scala.tools.refactoring.Compiler =>
  
  import global._
  
  class DeclarationIndex {
  
    private val defs = HashMap[Symbol, DefTree]()
    private val refs = HashMap[Symbol, ListBuffer[RefTree]]()
    private val children_ = HashMap[Symbol, ListBuffer[Symbol]]()
    
    private object defTreeTraverser extends Traverser {
      override def traverse(t: Tree) = {
        t match {
          case t: DefTree => 
            defs += t.symbol → t
            children_.getOrElseUpdate(t.symbol.owner, new ListBuffer[Symbol]) += t.symbol
          case t: RefTree => 
            refs.getOrElseUpdate(t.symbol, new ListBuffer[RefTree]) += t
          case _ => ()
        }
        super.traverse(t)
      }
    }
    
    // Symbol must be from a RefTree
    def declaration(s: Symbol) = defs(s)
    
    // Symbol must be from a DefTree. Does not yet include TypTrees.
    def references (s: Symbol) = refs(s)
    
    def children(s: Symbol): List[Symbol] = this.children_(s) toList
   
    def processTree(t: Tree) = defTreeTraverser traverse t
  }
}
