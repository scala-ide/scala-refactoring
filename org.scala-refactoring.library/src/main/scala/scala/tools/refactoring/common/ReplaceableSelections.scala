package scala.tools.refactoring.common

import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.transformation.TreeTransformations

trait ReplaceableSelections extends Selections with TreeTransformations {
  self: CompilerAccess =>

  import global._

  implicit class ReplaceableSelection(selection: Selection) {
    def descendToEnclosingTreeAndThen(trans: Transformation[Tree, Tree]) =
      topdown {
        matchingChildren {
          predicate { (t: Tree) =>
            t.samePosAndType(selection.enclosingTree)
          } &> trans
        }
      }

    private def replaceSingleStatementBy(replacement: Tree) = {
      val original = selection.selectedTopLevelTrees.head
      transform {
        case t if t.samePosAndType(original) =>
          replacement replaces t
      }
    }

    private def replaceSequenceBy(replacement: Tree, preserveHierarchy: Boolean) = {
      transform {
        case block @ Block(stats, expr) =>
          val allStats = (stats :+ expr)
          if (allStats.length == selection.selectedTopLevelTrees.length && !preserveHierarchy) {
            // only replace whole block if allowed to modify tree hierarchy
            replacement replaces block
          } else {
            val newStats = allStats.replaceSequencePreservingPositions(selection.selectedTopLevelTrees, replacement :: Nil)
            mkBlock(newStats) replaces block
          }
      }
    }

    /**
     * Replaces the selection by `replacement`.
     *
     * @param replacement
     * @param preserveHierarchy whether the original tree hierarchy must be preserved or
     *   could be reduced if possible.
     *   E.g. a selection contains all trees of the enclosing block:
     *   - with `preserveHierarchy = true` the block will be replaced by `replacement`
     *   - with `preserveHierarchy = false` the block will remain with `replacement`
     *     as its only child tree
     */
    def replaceBy(replacement: Tree, preserveHierarchy: Boolean = false) = {
      descendToEnclosingTreeAndThen {
        if (selection.selectedTopLevelTrees.length == 1)
          replaceSingleStatementBy(replacement)
        else
          replaceSequenceBy(replacement, preserveHierarchy)
      }
    }
  }
}