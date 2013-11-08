package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.common.CompilerAccess

trait Extractions extends VisibilityScopes with InsertionPoints { self: CompilerAccess =>
  import global._

  /**
   * An Extraction collects the required information to perform an extraction.
   * Use collectExtractions for construction of extractions.
   */
  trait Extraction {
    /** The selected code to extract */
    val selection: Selection
    
    /** The scope in which the extraction takes place */
    val scope: VisibilityScope
    
    /** The position where the new abstraction is going to be inserted */
    val abstractionPosition: InsertionPosition
    
    /** Inbound dependencies that are defined in this scope.
     *  This does not mean that those symbols are also visible
     *  from `abstractionPosition`. E.g. in block scopes
     */
    val definedDependencies: List[Symbol]

    /** Inbound dependencies that are not visible from this scope. */
    val undefinedDependencies = selection.inboundDeps diff definedDependencies

    /**
     * Transformation that inserts `insertion` in this scope at
     * `abstractionPosition`.
     */
    def insert(insertion: Tree) = {
      topdown {
        matchingChildren {
          transform {
            case t if t.samePosAndType(scope.enclosing) =>
              abstractionPosition(t)(insertion) replaces t
          }
        }
      }
    }

    /**
     * A list of transformations that perform the extraction by
     * replacing the selected code by `call` and insert `abstraction`
     * at `abstractionPosition`.
     */
    def extractionTransformations(call: Tree, abstraction: Tree) = {
      // Do the refactoring in two transformation steps in order to simplify
      // the extraction transformation
      selection.replaceBy(call) ::
        insert(abstraction) ::
        Nil
    }
  }

  object Extraction {
    type Filter = Extraction => Boolean

    def takesPlaceInA[T <: VisibilityScope](implicit m: Manifest[T]): Filter = { s =>
      m.runtimeClass.isInstance(s.scope)
    }

    val hasNoUndefinedDependencies: Filter = { s =>
      s.undefinedDependencies.isEmpty
    }

    val allExtractions: Filter = _ => true

    def insertionPositionApplicable(ip: InsertionPosition): Filter = { s =>
      ip.isDefinedAt(s.scope.enclosing)
    }
  }
}