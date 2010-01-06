package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.{HashMap, ListBuffer}

trait DeclarationIndexes {
    
  val global: scala.tools.nsc.Global  
  import global._
  
  class DeclarationIndex {
    
    private type Refs = ListBuffer[RefTree]
    private type Defs = ListBuffer[Symbol]
  
    private val defs = HashMap[Symbol, DefTree]()
    private val refs = HashMap[Symbol, Refs]()
    private val children_ = HashMap[Symbol, Defs]()
    
    private object defTreeTraverser extends Traverser {
      override def traverse(t: Tree) = {
        t match {
          case t: DefTree if t.symbol != NoSymbol => 
            defs += t.symbol → t
            children_.getOrElseUpdate(t.symbol.owner, new Defs) += t.symbol
          case t: RefTree => 
            refs.getOrElseUpdate(t.symbol, new Refs) += t
          case _ => ()
        }
        super.traverse(t)
      }
    }
    
    // Symbol must be from a RefTree
    def declaration(s: Symbol) = defs(s)
    
    // Symbol must be from a DefTree. Does not yet include TypTrees.
    def references (s: Symbol) = refs.getOrElse(s, new Refs) toList
    
    def children(s: Symbol): List[Symbol] = this.children_.getOrElse(s, new Defs) toList
   
    def processTree(t: Tree) = defTreeTraverser traverse t
    
    def clear {
      defs.clear()
      refs.clear()
      children_.clear()
    }
  }
}
