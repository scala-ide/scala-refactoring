package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.Selections

trait TreeAnalysis {
  
  self: Selections with DeclarationIndexes =>
  
  val global: scala.tools.nsc.Global
  
  protected val index: DeclarationIndex

  def inboundLocalDependencies(selection: TreeSelection, currentOwner: global.Symbol) = {
        
    val allLocals = index children currentOwner
    
    val selectedLocals = allLocals filter (selection.symbols contains)
          
    selectedLocals filterNot (s => selection contains (index declaration s))
  }
  
  def outboundLocalDependencies(selection: TreeSelection, currentOwner: global.Symbol) = {
    
    val allLocals = index children currentOwner
    
    val declarationsInTheSelection = allLocals filter (s => selection contains (index declaration s))
    
    declarationsInTheSelection flatMap (index references) filterNot (selection contains) map (_.symbol)
  }
}
