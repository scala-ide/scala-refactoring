package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.Selections

trait TreeAnalysis {
  
  self: scala.tools.refactoring.Compiler with Selections with DeclarationIndexes =>

  def inboundLocalDependencies(index: DeclarationIndex, selection: TreeSelection, currentOwner: global.Symbol) = {
    
    val allLocals = index children currentOwner
    
    val selectedLocals = allLocals filter (selection.symbols contains)
          
    selectedLocals filterNot (s => selection contains (index declaration s))
  }
}
