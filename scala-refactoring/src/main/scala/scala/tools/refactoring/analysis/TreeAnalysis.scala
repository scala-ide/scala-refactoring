/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.common.Selections

trait TreeAnalysis {
  
  self: Selections with Indexes with common.PimpedTrees /*really needed?*/ =>
  
  val global: scala.tools.nsc.interactive.Global
  
  def subSymbols(s: global.Symbol, index: IndexLookup): List[global.Symbol] = {
    
    def allChildren(t: global.Tree): List[global.Tree] = {
      t :: (children(t) flatMap allChildren)
    }
        
    val decls = index.declaration(s) toList
    
    decls flatMap allChildren filter {
      case t: global.SymTree => t.symbol != global.NoSymbol && t.symbol.ownerChain.contains(s)
      case _ => false
    } map (_.symbol) distinct
  }
  
  def inboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: IndexLookup) = {
        
    val allLocals = subSymbols(currentOwner, index)
    
    val selectedLocals = allLocals filter (selection.selectedSymbols contains)
          
    selectedLocals filterNot (s => index.declaration(s).map(selection.contains).headOption getOrElse false)
  }
  
  def outboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: IndexLookup) = {
    
    val allLocals = subSymbols(currentOwner, index)
    
    val declarationsInTheSelection = allLocals filter (s => index.declaration(s).map(selection.contains).headOption getOrElse false)
    
    val occurencesOfSelectedDeclarations = declarationsInTheSelection flatMap (index.occurences)
    
    occurencesOfSelectedDeclarations filterNot (selection.contains) map (_.symbol) distinct
  }
}
