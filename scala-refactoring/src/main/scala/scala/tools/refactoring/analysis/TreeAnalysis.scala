/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.refactoring.common.Selections

trait TreeAnalysis {
  
  self: Selections with Indexes =>
  
  val global: scala.tools.nsc.Global
  
  def inboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: Index) = {
        
    val allLocals = index children currentOwner map (_.symbol)
    
    val selectedLocals = allLocals filter (selection.selectedSymbols contains)
          
    selectedLocals filterNot (s => index.declaration(s) map selection.contains getOrElse false)
  }
  
  def outboundLocalDependencies(selection: Selection, currentOwner: global.Symbol, index: Index) = {
    
    val allLocals = index children currentOwner map (_.symbol)
    
    val declarationsInTheSelection = allLocals filter (s => index.declaration(s) map selection.contains getOrElse false)
    
    declarationsInTheSelection flatMap (index references) filterNot (selection contains) map (_.symbol) distinct
  }
}
