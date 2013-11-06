package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.common.CompilerAccess

trait ExtractionScopes extends VisibilityScopes with InsertionPoints { self: CompilerAccess =>
  import global._

  /**
   * Tracks inbound dependencies of a selection in a visibility scope tree
   * and offers consistent insertion transformations for extraction refactorings.
   * Use collectExtractionScopes for construction of extraction scopes.
   */
  case class ExtractionScope(
    selection: Selection,
    scope: VisibilityScope,
    insertionPoint: InsertionPoint,
    definedDependencies: List[Symbol],
    undefinedDependencies: List[Symbol]) {

    def insert(insertion: Tree) = {
      topdown {
        matchingChildren {
          transform {
            case t if t.samePosAndType(scope.enclosing) =>
              insertionPoint(t)(insertion) replaces t
          }
        }
      }
    }
  }

  object ExtractionScope {
    type Filter = ExtractionScope => Boolean

    def isA[T <: VisibilityScope](implicit m: Manifest[T]): Filter = { s =>
      m.runtimeClass.isInstance(s.scope)
    }

    val hasNoUndefinedDependencies: Filter = { s =>
      s.undefinedDependencies.isEmpty
    }

    val allScopes: Filter = _ => true

    def matchesInsertionPoint(ip: InsertionPoint): Filter = { s =>
      ip.isDefinedAt(s.scope.enclosing)
    }
  }

  def collectExtractionScopes(selection: Selection, ip: InsertionPoint, filter: ExtractionScope.Filter) = {
    val vs = VisibilityScope(selection)
    val inboundDeps = {
      val usedSymbols = selection.selectedSymbols
      val definedSymbols = selection.allSelectedTrees.collect {
        case t: DefTree => t.symbol
      }
      usedSymbols.diff(definedSymbols)
    }

    val scopeFilter = { s: ExtractionScope =>
      filter(s) && ExtractionScope.matchesInsertionPoint(ip)(s)
    }

    def inner(vs: VisibilityScope, undefinedDeps: List[Symbol]): List[ExtractionScope] = {
      val definedInVs = vs.symbols intersect inboundDeps
      val es = ExtractionScope(selection, vs, ip, inboundDeps diff undefinedDeps, undefinedDeps)
      val scopes =
        if (scopeFilter(es))
          es :: Nil
        else Nil
      vs.visibleScopes match {
        case Nil => scopes
        case children => scopes ::: children.flatMap(inner(_, undefinedDeps union definedInVs))
      }
    }

    inner(vs, Nil)
  }
}