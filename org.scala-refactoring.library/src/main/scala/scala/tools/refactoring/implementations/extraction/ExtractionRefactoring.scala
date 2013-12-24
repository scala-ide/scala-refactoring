package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.transformation.TransformableSelections
import scala.tools.refactoring.analysis.ScopeAnalysis
import scala.tools.refactoring.common.InsertionPositions

/**
 * A supertrait for extraction refactorings.
 *
 * Subclasses of this trait are only adapters that implement
 * the MultiStageRefactoring interface. The refactoring logic
 * itself is provided by `Extractions`.
 *
 * An extraction refactoring computes in the preparation phase
 * a list of possible extractions based on the current selection.
 */
trait ExtractionRefactoring extends MultiStageRefactoring with Extractions {
  case class PreparationResult(extractions: List[Extraction])

  type RefactoringParameters = Extraction

  def prepare(s: Selection) =
    collectExtractions(s)
      .left.map(PreparationError(_))
      .right.map(PreparationResult(_))

  def perform(selectedExtraction: Extraction) = {
    val transformations = selectedExtraction.perform()
    Right(transformFile(selectedExtraction.extractionSource.file, transformations))
  }

  def perform(s: Selection, prepared: PreparationResult, extraction: RefactoringParameters) =
    perform(extraction)
}

/**
 * Base trait for modules that offer a specific kind of extractions.
 */
trait Extractions extends ScopeAnalysis with TransformableSelections with InsertionPositions with CompilerAccess {
  import global._

  /**
   * A concrete and applicable extraction.
   */
  trait Extraction {
    val extractionSource: Selection

    val extractionTarget: ExtractionTarget

    /**
     * A brief description of the extraction.
     */
    val displayName: String

    def perform(): List[Transformation[Tree, Tree]]

    def withAbstractionName(name: String): this.type
  }

  case class ExtractionTarget(scope: ScopeTree, enclosing: Tree, ip: InsertionPosition) {
    /**
     * Approximated position where new trees are inserted.
     */
    lazy val pos = ip(enclosing).pos

    def insert(t: Tree): Transformation[Tree, Tree] =
      topdown {
        matchingChildren {
          transform {
            case e if e.samePosAndType(enclosing) =>
              ip(e)(t)
          }
        }
      }
  }

  type ErrorMsg = String
  
  val defaultAbstractionName = "extracted"

  /**
   * Collects a list of extractions that are applicable for selection `s`
   * If no extraction is applicable an appropriate error message is returned.
   */
  def collectExtractions(s: Selection): Either[ErrorMsg, List[Extraction]] = {
    def inner(source: Selection) = {
      val ip = prepareInsertionPosition(source)
      val scopes = ScopeTree.build(source)
      val mkTarget = prepareExtractionTarget(ip, scopes)_
      val targets = source
        .filterSelected(t => !t.pos.sameRange(source.pos))
        .reverse
        .flatMap { enclosing =>
          mkTarget(enclosing)
        }

      prepareExtractions(source, targets)
    }

    for {
      source <- prepareExtractionSource(s).right
      extractions <- inner(source).right
    } yield extractions
  }

  val noExtractionMsg = "No insertion position found."

  /**
   * Expands the selection `s` if necessary or returns an error message if no
   * extraction is not applicable.
   */
  def prepareExtractionSource(s: Selection): Either[ErrorMsg, Selection]

  def findExtractionSource(s: Selection)(pred: Selection => Boolean): Option[Selection] =
    if (pred(s))
      Some(s)
    else
      s.expandToNextEnclosingTree.flatMap(findExtractionSource(_)(pred))

  def prepareInsertionPosition(s: Selection): InsertionPosition = {
    s.beforeSelectionInBlock orElse
      s.afterSelectionInTemplate orElse
      atBeginningOfNewDefBody orElse
      atBeginningOfNewFunctionBody orElse
      atBeginningOfCaseBody
  }

  def prepareExtractionTarget(ip: InsertionPosition, scopes: ScopeTree)(t: Tree): List[ExtractionTarget] = {
    if (ip.isDefinedAt(t)) {
      val scope = scopes.findScopeFor(ip(t).pos)
      ExtractionTarget(scope, t, ip) :: Nil
    } else {
      Nil
    }
  }

  def prepareExtractions(source: Selection, targets: List[ExtractionTarget]): Either[ErrorMsg, List[Extraction]]
}