package scala.tools.refactoring.common

import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.transformation.TreeTransformations

trait ReplaceableSelections extends Selections with TreeTransformations {
  self: CompilerAccess =>

  import global._
  import PartialFunction._

  /**
   * Helper methods for expansion of selections to new selections
   * that encloses a specific statement or sequence of satements.
   */
  implicit class ExpandableSelection(selection: Selection) {
    def expandTo(newPos: Position): Option[Selection] = {
      if (newPos.isRange)
        Some(new Selection {
          val root = selection.root
          val file = selection.file
          val pos = newPos.asInstanceOf[RangePosition]
        })
      else
        None
    }

    def expandTo(tree: Tree): Option[Selection] =
      expandTo(tree.pos)

    def expandTo[T <: Tree](implicit m: Manifest[T]): Option[Selection] = {
      val newSelTree = selection.findSelectedOfType[T]
      newSelTree.flatMap(expandTo(_))
    }

    private def selectionMatchesFirstTree =
      selection.selectedTopLevelTrees.headOption.map { firstTree =>
        firstTree.pos.start == selection.pos.start && firstTree.pos.end == selection.pos.end
      }.getOrElse(false)

    private def intersectingPositions(p1: Position, p2: Position) =
      p1.isRange && p2.isRange && {
        val (first, second) =
          if (p1.start <= p2.start) (p1, p2) else (p2, p1)
        first.end > second.start
      }

    def expand: Selection =
      if (selectionMatchesFirstTree) {
        selection
      } else {
        def posOfPartiallySelectedTrees(trees: List[Tree], newPos: Position = selection.pos): Position =
          trees match {
            case t :: rest if t.pos overlaps selection.pos =>
              posOfPartiallySelectedTrees(rest, newPos union t.pos)
            case t :: rest =>
              posOfPartiallySelectedTrees(rest, newPos)
            case Nil => newPos
          }

        expandTo(posOfPartiallySelectedTrees(selection.enclosingTree.children)).getOrElse(selection)
      }

  }

  implicit class SelectionProperties(selection: Selection) {
    lazy val definesNonLocal = selection.selectedTopLevelTrees.exists(cond(_) {
      case t: DefTree => !t.symbol.isLocal
    })

    lazy val definesNonValue = selection.selectedTopLevelTrees.exists(cond(_) {
      case t: DefTree => !t.symbol.isType
    })
  }

  implicit class ReplaceableSelection(selection: Selection) {
    private def descendToEnclosingTreeAndThen(trans: Transformation[Tree, Tree]) =
      topdown {
        matchingChildren {
          predicate { (t: Tree) =>
            t == selection.enclosingTree
          } &> trans
        }
      }

    private def replaceSingleStatementBy(replacement: Tree) = {
      val substitution = replaceTree(selection.selectedTopLevelTrees.head, replacement)
      substitution |> topdown {
        matchingChildren {
          substitution
        }
      }
    }

    private def replaceSequenceBy(replacement: Tree) = {
      transform {
        case block @ Block(stats, expr) =>
          val allStats = (stats :+ expr)
          if (allStats.length == selection.selectedTopLevelTrees.length) {
            replacement replaces block
          } else {
            val newStats = (stats :+ expr).replaceSequence(selection.selectedTopLevelTrees, replacement :: Nil)
            mkBlock(newStats) replaces block
          }
      }
    }

    def replaceBy(replacement: Tree) =
      descendToEnclosingTreeAndThen {
        if (selection.selectedTopLevelTrees.length == 1)
          replaceSingleStatementBy(replacement)
        else
          replaceSequenceBy(replacement)
      }

  }
}