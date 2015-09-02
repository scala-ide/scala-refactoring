/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.reflect.internal.util.RangePosition
import scala.util.control.NonFatal

abstract class MarkOccurrences extends common.Selections with analysis.Indexes with common.CompilerAccess with common.PimpedTrees {

  val global: tools.nsc.interactive.Global
  import global._

  def occurrencesOf(file: tools.nsc.io.AbstractFile, from: Int, to: Int): (Tree, List[Position]) = {

    def positionOverlapsSelection(p: Position) = (
          ((new RangePosition(null, from, from, to)) overlaps p) ||
          (from == to && ( to == p.end || to == p.start))
      )

    def occurrencesForSymbol(s: Symbol) = {
      val occurrences = index.occurences(s)
      occurrences map (_.namePosition) filter  (_ != global.NoPosition)
    }

    val selectedTree = (new FileSelection(file, global.unitOfFile(file).body, from, to)).findSelectedWithPredicate {
      case (_: global.TypeTree) | (_: global.SymTree) | (_: global.Ident) => true
      case _ => false
    }

    val occurrences = selectedTree.toList flatMap {

      // for example, in List[String], String is an Ident and needs special treatment
      case t: Ident if t.symbol == null || t.symbol == NoSymbol =>
        val symbols = index positionToSymbol t.pos

        symbols flatMap occurrencesForSymbol

      case imp: Import =>

        imp.Selectors().find { selector =>
          positionOverlapsSelection(selector.pos)
        }.toList.flatMap { selector =>
          findSymbolForImportSelector(imp.expr, selector.name.name)
        }.map { sym =>
          occurrencesForSymbol(sym)
        }.flatten

      case selectedLocal =>
        // source files that contain errors can lead to
        // position exceptions in various places, so we
        // just catch everything here
        val namePos = try {
          selectedLocal.namePosition
        } catch {
          case NonFatal(_) => NoPosition
        }
        val symbol = selectedLocal.symbol

        if(symbol == null || namePos == NoPosition || !positionOverlapsSelection(namePos)) {
          Nil
        } else {
          occurrencesForSymbol(symbol)
        }
    }

    (selectedTree getOrElse EmptyTree, occurrences)
  }
}
