package scala.tools.refactoring.transformation

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.common.InsertionPositions

trait TransformableScopes extends VisibilityScopes with InsertionPositions with Selections { self: CompilerAccess =>
  import global._

  /**
   * Extends VisibilityScopes with some tree transformations.
   */
  implicit class TransformableScope(scope: VisibilityScope) {

    /**
     * Inserts `tree` in the visibility scope.
     * It is guaranteed, that the tree is inserted at a position,
     * from which all symbols in `scope.allVisibleSymbols` are
     * accessible if `defaultInsertionPosition` is used.
     */
    def insert(tree: Tree, ip: InsertionPosition = defaultInsertionPosition): Transformation[Tree, Tree] =
      topdown {
        matchingChildren {
          transform {
            case e if e.samePosAndType(scope.enclosing) =>
              ip(e)(tree) replaces e
          }
        }
      }

    val defaultInsertionPosition =
      scope.referenceSelection.beforeSelectionInBlock orElse
        scope.referenceSelection.afterSelectionInTemplate orElse
        atBeginningOfDefDef orElse
        atBeginningOfFunction orElse
        atBeginningOfCaseBody
  }
}