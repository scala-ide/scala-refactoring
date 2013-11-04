package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.common.CompilerAccess

trait ExtractionScopes extends VisibilityScopes { self: CompilerAccess =>
  import global._

  /**
   * Tracks inbound dependencies of a selection in a visibility scope tree
   * and offers consistent insertion transformations for extraction refactorings.
   * Use collectExtractionScopes for construction of extraction scopes.
   */
  case class ExtractionScope(
    scope: VisibilityScope,
    definedDependencies: List[Symbol],
    undefinedDependencies: List[Symbol]) {

    def findScopeAndThen(trans: Transformation[Tree, Tree]) = topdown {
      matchingChildren {
        predicate((t: Tree) => t.samePosAndType(scope.enclosing))
      }
    }

    def insert(t: Tree) =
      findScopeAndThen {
        fail[Tree]
      }
  }

  def collectExtractionScopes(selection: Selection) = {
    val vs = VisibilityScope(selection)
    val inboundDeps = {
      val usedSymbols = selection.selectedSymbols
      val definedSymbols = selection.allSelectedTrees.collect {
        case t: DefTree => t.symbol
      }
      usedSymbols.diff(definedSymbols)
    }

    def inner(vs: VisibilityScope, undefinedDeps: List[Symbol]): List[ExtractionScope] = {
      val definedInVs = vs.symbols
      val es = ExtractionScope(vs, inboundDeps diff undefinedDeps, undefinedDeps)
      vs.visibleScopes match {
        case Nil => es :: Nil
        case children => es :: children.flatMap(inner(_, undefinedDeps union definedInVs))
      }
    }

    inner(vs, Nil)
  }
}