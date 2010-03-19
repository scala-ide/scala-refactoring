package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.common.Selections

trait TreeAnalysis {
  
  self: Selections with Indexes =>
  
  val global: scala.tools.nsc.Global
  
  def inboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: Index) = {
        
    val allLocals = index children currentOwner map (_.symbol)
    
    val selectedLocals = allLocals filter (selection.selectedSymbols contains)
          
    selectedLocals filterNot (s => selection contains (index declaration s))
  }
  
  def outboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: Index) = {
    
    val allLocals = index children currentOwner map (_.symbol)
    
    val declarationsInTheSelection = allLocals filter (s => selection contains (index declaration s))
    
    declarationsInTheSelection flatMap (index references) filterNot (selection contains) map (_.symbol) distinct
  }
}
