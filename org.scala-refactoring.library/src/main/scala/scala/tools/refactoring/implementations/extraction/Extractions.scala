package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.common.CompilerAccess

trait Extractions extends VisibilityScopes with InsertionPoints { self: CompilerAccess =>
  import global._

  /**
   * An Extraction collects the required information to perform an extraction.
   * Use collectExtractions for construction of extractions.
   *
   * @param selection The selected code that should be extracted
   * @param scope The visibility scope in which the extraction takes place
   *   and the new abstractions is going to be inserted
   * @param abstractionPosition The relative position in the visibility scope
   *   where the new abstraction is going to be inserted. E.g. before the selection
   * @param definedDependencies Inbound dependencies to the selected code that are visible
   *   in the visibility scope
   * @param undefinedDependencies Inbound dependencies that are not visible in the
   *   visibility scope
   */
  case class Extraction(
    selection: Selection,
    scope: VisibilityScope,
    abstractionPosition: InsertionPosition,
    definedDependencies: List[Symbol],
    undefinedDependencies: List[Symbol]) {

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

    val allScopes: Filter = _ => true

    def matchesInsertionPoint(ip: InsertionPosition): Filter = { s =>
      ip.isDefinedAt(s.scope.enclosing)
    }
  }

  def collectExtractions(selection: Selection, ip: InsertionPosition, filter: Extraction.Filter) = {
    val vs = VisibilityScope(selection)

    val extractionFilter = { s: Extraction =>
      filter(s) && Extraction.matchesInsertionPoint(ip)(s)
    }

    def inner(vs: VisibilityScope, undefinedDeps: List[Symbol]): List[Extraction] = {
      val definedInVs = vs.symbols intersect selection.inboundDeps
      val e = Extraction(selection, vs, ip, selection.inboundDeps diff undefinedDeps, undefinedDeps)
      val extraction = if (extractionFilter(e)) e :: Nil else Nil
      vs.visibleScopes match {
        case Nil => extraction
        case children => extraction ::: children.flatMap(inner(_, undefinedDeps union definedInVs))
      }
    }

    inner(vs, Nil)
  }
}