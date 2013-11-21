package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.transformation.TreeTransformations
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
     * accessible.
     */
    def insert(tree: Tree): Transformation[Tree, Tree] =
      topdown {
        matchingChildren {
          transform {
            case t if t.samePosAndType(scope.enclosing) =>
              defaultInsertionPosition(scope.enclosing)(tree) replaces t
          }
        }
      }

    val defaultInsertionPosition =
      scope.referenceSelection.beforeSelectionInBlock orElse
        scope.referenceSelection.beforeSelectionInTemplate orElse
        atBeginningOfDefDef orElse
        atBeginningOfFunction orElse
        atBeginningOfCaseBody
  }
}