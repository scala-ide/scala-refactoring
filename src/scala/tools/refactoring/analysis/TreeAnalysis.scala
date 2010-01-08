package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.util.Selections

trait TreeAnalysis {
  
  self: Selections with Indexes =>
  
  val global: scala.tools.nsc.Global
  
  protected val index: Index

  def inboundLocalDependencies(selection: TreeSelection, currentOwner: global.Symbol) = {
        
    val allLocals = index children currentOwner map (_.symbol)
    
    val selectedLocals = allLocals filter (selection.symbols contains)
          
    selectedLocals filterNot (s => selection contains (index declaration s))
  }
  
  def outboundLocalDependencies(selection: TreeSelection, currentOwner: global.Symbol) = {
    
    val allLocals = index children currentOwner map (_.symbol)
    
    val declarationsInTheSelection = allLocals filter (s => selection contains (index declaration s))
    
    declarationsInTheSelection flatMap (index references) filterNot (selection contains) map (_.symbol)
  }
}
