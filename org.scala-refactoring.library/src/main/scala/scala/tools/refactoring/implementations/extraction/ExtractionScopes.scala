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

  class ExtractionScopePredicate(val p: ExtractionScope => Boolean, val str: String = "<unknown>") {
    def &&(q: ExtractionScopePredicate) =
      new ExtractionScopePredicate(s => p(s) && q.p(s), "(" + str + " && " + q.str + ")")

    def ||(q: ExtractionScopePredicate) =
      new ExtractionScopePredicate(s => p(s) || q.p(s), "(" + str + " || " + q.str + ")")

    def unary_! =
      new ExtractionScopePredicate(s => !p(s), "!" + str)

    def apply(es: ExtractionScope) = p(es)

    override def toString = str
  }

  def isA[T <: VisibilityScope](implicit m: Manifest[T]) =
    new ExtractionScopePredicate(s => m.runtimeClass.isInstance(s.scope), "isA[" + m.runtimeClass.getSimpleName() + "]")

  def hasNoUndefinedDependencies =
    new ExtractionScopePredicate(s => s.undefinedDependencies.isEmpty, "noUndefinedDeps")

  def allScopes =
    new ExtractionScopePredicate(_ => true)

  def collectExtractionScopes(selection: Selection, ip: InsertionPoint, pred: ExtractionScopePredicate = allScopes) = {
    val vs = VisibilityScope(selection)
    val inboundDeps = {
      val usedSymbols = selection.selectedSymbols
      val definedSymbols = selection.allSelectedTrees.collect {
        case t: DefTree => t.symbol
      }
      usedSymbols.diff(definedSymbols)
    }

    val shouldUseScope = pred && !isA[PackageScope]

    def inner(vs: VisibilityScope, undefinedDeps: List[Symbol]): List[ExtractionScope] = {
      val definedInVs = vs.symbols intersect inboundDeps
      val es = ExtractionScope(selection, vs, ip, inboundDeps diff undefinedDeps, undefinedDeps)
      val scopes = if (shouldUseScope(es)) es :: Nil else Nil
      vs.visibleScopes match {
        case Nil => scopes
        case children => scopes ::: children.flatMap(inner(_, undefinedDeps union definedInVs))
      }
    }

    inner(vs, Nil)
  }
}