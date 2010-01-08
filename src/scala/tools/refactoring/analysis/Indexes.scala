package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols
import scala.collection.mutable.{HashMap, ListBuffer}

trait Indexes {

  val global: scala.tools.nsc.Global  
  import global._
  
  object index {
    
    private type Refs = ListBuffer[RefTree]
    private type Defs = ListBuffer[DefTree]
  
    private val defs = new HashMap[Symbol, DefTree]
    private val refs = new HashMap[Symbol, Refs]
    private val chld = new HashMap[Symbol, Defs]

    // Symbol must be from a RefTree
    def declaration(s: Symbol) = defs(s)
    
    // Symbol must be from a DefTree. Does not yet include TypTrees.
    def references(s: Symbol) = refs.getOrElse(s, new Refs) toList
    
    def children(s: Symbol) = chld.getOrElse(s, new Defs) toList
   
    def processTree(t: Tree) = t foreach {
      case t: DefTree if t.symbol != NoSymbol => 
        defs += t.symbol → t
        chld.getOrElseUpdate(t.symbol.owner, new Defs) += t
      case t: RefTree => 
        refs.getOrElseUpdate(t.symbol, new Refs) += t
      case _ => ()
    }
  }
}
