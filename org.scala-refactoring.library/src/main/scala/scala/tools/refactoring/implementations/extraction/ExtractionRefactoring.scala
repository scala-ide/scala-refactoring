package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.ReplaceableSelections
import scala.tools.refactoring.analysis.VisibilityScopes
import scala.tools.refactoring.analysis.TreeAnalysis

trait ExtractionRefactoring extends MultiStageRefactoring with CompilerAccess with Abstractions with InsertionPoints
  with VisibilityScopes {
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

    /**
     * Inbound dependencies that are defined in this scope.
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

  /**
   * Creates an insertion position that inserts local values
   * before the selection `s` and class members after the
   * method that contains the selection.
   */
  val defaultInsertionPositionFor = (s: Selection) =>
    s.beforeSelectionInBlock orElse
      s.afterSelectionInTemplate orElse
      atBeginningOfDefDef orElse
      atBeginningOfFunction

  /**
   * Tries to find a valid selection that is replaceable by a expression.
   * Returns either a message why the selection is not valid or a selection
   * that has all required properties.
   */
  def ensureExpressionsSelected(s: Selection): Either[PreparationError, Selection] = {
    val expanded = s.expand
    if (expanded.definesNonLocal)
      Left(PreparationError("Cannot extract selection that defines non local fields."))
    else if (expanded.definesNonValue)
      Left(PreparationError("Cannot extract selection that defines non value symbols."))
    else if (expanded.containsImportStatements)
      Left(PreparationError("Cannot extract selection that contains import statements."))
    else
      Right(expanded)
  }

  /**
   * Collects all extractions for `s` that are applicable at insertion position `ip`.
   */
  def collectExtractions(s: Selection, ip: InsertionPosition): Either[PreparationError, List[Extraction]] = {
    class ExtractionImpl(
      val selection: Selection,
      val scope: VisibilityScope,
      val abstractionPosition: InsertionPosition,
      val definedDependencies: List[Symbol]) extends Extraction

    val vs = VisibilityScope(s)

    def inner(vs: VisibilityScope, definedDeps: List[Symbol]): List[Extraction] = {
      val extraction = if (ip.isDefinedAt(vs.enclosing))
        Some(new ExtractionImpl(s, vs, ip, definedDeps))
      else
        None
      vs.visibleScopes match {
        case Nil => extraction.toList
        case children => extraction.toList ::: children.flatMap(inner(_, definedDeps diff vs.symbols))
      }
    }

    inner(vs, s.inboundDeps) match {
      case Nil => Left(PreparationError("No insertion points found."))
      case es => Right(es)
    }
  }

  /**
   * Filters all extraction scopes that have undefined dependencies.
   */
  def ensureNoUndefinedDependencies(es: List[Extraction]): Either[PreparationError, List[Extraction]] = {
    es.filter(_.undefinedDependencies.isEmpty) match {
      case Nil => Left(PreparationError("Selection has dependencies that are not visible from any insertion points."))
      case es => Right(es)
    }
  }
}