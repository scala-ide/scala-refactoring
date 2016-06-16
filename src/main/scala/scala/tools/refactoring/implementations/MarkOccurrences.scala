/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import scala.reflect.internal.util.OffsetPosition
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.common.PositionDebugging
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.util.control.NonFatal
import scala.tools.refactoring.util.SourceWithMarker.Movement

trait MarkOccurrences extends common.Selections with analysis.Indexes with common.CompilerAccess with common.EnrichedTrees with common.InteractiveScalaCompiler {
  import global._

  protected class SingleTreeSelection(val selected: Tree, val root: Tree) {
    val symbol = selected match {
      case Import(expr, List(selector)) =>
        findSymbolForImportSelector(expr, selector.name).getOrElse(NoSymbol)
      case tree => tree.symbol
    }

    val name = {
      // We try to read the old name from the name position of the original symbol, since this seems to be the most
      // reliable strategy that also works well in the presence of `backtick-identifiers`.
      index.declaration(symbol).flatMap { tree =>
        tree.namePosition() match {
          case rp: RangePosition => Some(rp.source.content.slice(rp.start, rp.end).mkString(""))
          case _ => None
        }
      }.getOrElse {
        trace("Cannot find old name of symbol reliably; attempting fallback")
        try {
          selected match {
            case Import(_, List(selector)) => selector.rename.toString
            case _ => selected match {
              case s: Select => s.name.toString()
              case other => other.nameString
            }
          }
        } catch {
          case NonFatal(e) =>
            trace("Cannot find old name of symbol; giving up")
            "<no-name>"
        }
      }
    } \\ { name =>
      trace(s"SingleTreeSelection.name: $name")
    }
  }

  protected def findOccurrences(selection: SingleTreeSelection): List[(RangePosition, Tree)] = {
    /*
     * A lazy val is represented by a ValDef and an associated DefDef that contains the initialization code.
     * Unfortunately, the Scala compiler does not set modifier positions for the DefDef, which is a problem for us,
     * because we are relying on the DefDef when printing out source code. The following function patches the affected
     * DefDefs accordingly.
     *
     * See #1002392 and #1002502
     */
    def eventuallyFixModifierPositionsForLazyVals(t: Tree): Unit = t match {
      case dd: DefDef if dd.mods.isLazy && dd.mods.positions.isEmpty =>
        val vd = selection.root.find {
          case vd: ValDef if vd.mods.isLazy && !vd.mods.positions.isEmpty && dd.pos.point == vd.pos.point => true
          case _ => false
        }

        vd.foreach { vd =>
          // Note that we patch the DefDef in place because we need the correctly set positions in the tree
          // later in the refactoring process. A precursor of this code pretended to be pure, by copying
          // dd and then calling 'mods.setPositions' on the copy. This had exactly the same (needed) side effects
          // however, as the Modifier object affected by setPositions was the same anyway. If you are interested
          // in the related discussion, take a look at https://github.com/scala-ide/scala-refactoring/pull/78.
          dd.mods.setPositions(vd.asInstanceOf[ValDef].mods.positions)
        }

      case _ => ()
    }

    /*
     * ImportSelectorTrees need special treatment, since it is not a priory clear which side (the name or rename side) of
     * the tree is relevant for us.
     */
    def positionsWithPotentiallyReferencingTrees(trees: Seq[Tree]): Seq[(Position, Tree)] = {
      trees.flatMap {
        case ImportSelectorTree(name, rename) => Seq(name.namePosition() -> name, rename.namePosition() -> rename)
        case other => Seq(other.namePosition() -> other)
      }
    }

    val referencingTrees = index.occurences(selection.symbol)

    referencingTrees.foreach { s =>
      trace("Symbol is referenced at %s", PositionDebugging.formatCompact(s.pos))
      eventuallyFixModifierPositionsForLazyVals(s)
    }

    import Movements._
    val mvToSymStartForRangePos = Movements.until(selection.name, skipping = (comment | space | reservedName))
    val occurences = positionsWithPotentiallyReferencingTrees(referencingTrees).flatMap { case (pos, occ) =>
      pos match {
        case np: RangePosition =>
          // Unfortunately, there are cases when the name position cannot be used directly.
          // Therefore we have to use appropriate movements to make sure we capture the correct range.
          val srcAtStart = SourceWithMarker.atStartOf(np)
          mvToSymStartForRangePos(srcAtStart).flatMap { markerAtSymStart =>
            val consumedId = Movement.coveredString(markerAtSymStart, srcAtStart.source, Movements.id)

            if (consumedId == selection.name) {
              Some((new RangePosition(np.source, markerAtSymStart, markerAtSymStart, markerAtSymStart + selection.name.length), occ))
            } else {
              None
            }
          }

        case op: OffsetPosition =>
          // Normally, we would not like to deal with offset positions here at all.
          // Unfortunately the compiler emits them instead of range positions in
          // interpolated strings like f"$x" (see #1002651).
          val pointIsAtStartOfNameToBeChanged = {
            val srcBeforePoint = SourceWithMarker.atPoint(op).step(forward = false)
            val pointIsInMiddleOfId = Movements.id(srcBeforePoint).exists(_ > op.point)

            if (pointIsInMiddleOfId) {
              false
            } else {
              val srcAtPoint = srcBeforePoint.step(forward = true)
              val consumedId = Movement.coveredString(srcAtPoint, Movements.id)
              consumedId == selection.name
            }
          }

          if (pointIsAtStartOfNameToBeChanged) {
            Some((new RangePosition(op.source, op.point, op.point, op.point + selection.name.length), occ))
          } else {
            None
          }

        case _ =>
          None
      }
    }

    // Since the ASTs do not directly represent the user source code, it might be easily possible that
    // some ranges are duplicated. The code below removes them.
    val uniqOccurences = occurences.groupBy(_._1).map { case (pos, occWithTrees) =>
      (pos, occWithTrees.head._2)
    }.toList

    uniqOccurences
  }

  def occurrencesOf(file: tools.nsc.io.AbstractFile, from: Int, to: Int): (Tree, List[Position]) = {
    val selection = new FileSelection(file, global.unitOfFile(file).body, from, to)

    val selectedTree = selection.findSelectedWithPredicate {
      case (_: global.TypeTree) | (_: global.SymTree) | (_: global.Ident) => true
      case _ => false
    }

    val cursorOnScalaId = {
      val positionsToCheck = selectedTree.toSeq.flatMap {
        case imp: Import =>
          // Imports need special handling, since it is not clear on which part of the import statement the cursor might be positioned on
          Seq(imp.expr.namePosition()) ++ imp.selectors.map(s => new OffsetPosition(imp.pos.source, s.namePos))

        case other => Seq(other.namePosition())
      }

      positionsToCheck.exists {
        case rp: RangePosition => rp.start >= from && rp.end <= to
        case op: OffsetPosition => op.point >= from && op.point <= to
        case _ => false
      }
    }

    if (!cursorOnScalaId) {
      (EmptyTree, Nil)
    } else {
      val occurrences = selectedTree.toList.flatMap { selectedTree =>
        val singleTreeSelection = new SingleTreeSelection(selectedTree, selection.root)
        val occurences = findOccurrences(singleTreeSelection)
        occurences.map(_._1)
      }

      (selectedTree.getOrElse(EmptyTree), occurrences)
    }
  }
}
