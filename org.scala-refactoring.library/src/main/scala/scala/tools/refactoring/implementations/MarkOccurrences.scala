/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import tools.nsc.io.AbstractFile
import tools.nsc.util.RangePosition

abstract class MarkOccurrences extends common.Selections with analysis.Indexes with common.CompilerAccess with common.PimpedTrees {
    
  val global: tools.nsc.interactive.Global
  import global._
  
  def occurrencesOf(file: AbstractFile, from: Int, to: Int): (Tree, List[Position]) = {
    
    def positionOverlapsSelection(p: Position) = (
          ((new RangePosition(null, from, from, to)) overlaps p) ||
          from == to && to == p.end
      )
    
    def occurrencesForSymbol(s: Symbol) = 
      index occurences s map (_.namePosition) filter (_ != global.NoPosition)
    
    val selectedTree = (new FileSelection(file, from, to)).findSelectedWithPredicate {
      case (_: global.TypeTree) | (_: global.SymTree) | (_: global.Ident) => true
      case _ => false
    }
    
    val occurrences = selectedTree.toList flatMap {
      
      // for example, in List[String], String is an Ident and needs special treatment 
      case t: Ident if t.symbol == null || t.symbol == NoSymbol =>
        val symbols = index positionToSymbol t.pos
        
        symbols flatMap occurrencesForSymbol
        
      case selectedLocal =>
        val namePos = selectedLocal.namePosition
        val symbol = selectedLocal.symbol
        
        if(symbol == null || namePos == NoPosition || !positionOverlapsSelection(namePos))
          return (EmptyTree, Nil)
        
        occurrencesForSymbol(symbol)
    }
    
    (selectedTree getOrElse EmptyTree, occurrences)
  }
}
