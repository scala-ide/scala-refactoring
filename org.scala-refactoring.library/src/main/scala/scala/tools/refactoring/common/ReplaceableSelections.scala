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

    def expand: Option[Selection] =
      if (selectionMatchesFirstTree) {
        Some(selection)
      } else {
        def posOfPartiallySelectedTrees(trees: List[Tree], newPos: Position = selection.pos): Position =
          trees match {
            case t :: rest if !t.pos.isRange || t.pos.end < selection.pos.start || t.pos.start > selection.pos.end =>
              posOfPartiallySelectedTrees(rest, newPos)
            case t :: rest =>
              posOfPartiallySelectedTrees(rest, newPos union t.pos)
            case Nil => newPos
          }

        expandTo(posOfPartiallySelectedTrees(selection.enclosingTree.children))
      }

  }

  implicit class SelectionProperties(selection: Selection) {
    lazy val definesNonLocal = selection.selectedTopLevelTrees.exists(cond(_) {
      case t: DefTree => !t.symbol.isLocal
    })

    lazy val definesNonValue = selection.selectedTopLevelTrees.exists(cond(_) {
      case t: DefTree => !t.symbol.isType
    })

    lazy val enclosingTree =
      selection.findSelectedWithPredicate {
        case t => t != selection.selectedTopLevelTrees.head
      }.getOrElse(selection.root)
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