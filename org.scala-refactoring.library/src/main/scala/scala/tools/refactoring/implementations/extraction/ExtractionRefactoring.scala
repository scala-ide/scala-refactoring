package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.transformation.TransformableScopes
import scala.tools.refactoring.transformation.TransformableSelections

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
  type E <: Extraction

  val collector: ExtractionCollector[E]

  case class PreparationResult(extractions: List[E])

  def prepare(s: Selection) =
    collector.collectExtractions(s)
      .left.map(PreparationError(_))
      .right.map(PreparationResult(_))
  
  def perform(selectedExtraction: E, name: String) = {
    val transformations = selectedExtraction.perform(name)
    Right(transformFile(selectedExtraction.scope.referenceSelection.file, transformations))
  }
}

/**
 * Base trait for modules that offer a specific kind of extractions.
 */
trait Extractions extends TransformableScopes with TransformableSelections with Abstractions with CompilerAccess {
  import global._

  /**
   * A concrete and applicable extraction.
   */
  trait Extraction {
    val scope: VisibilityScope

    val extractionSource: Selection

    val name: String

    def perform(abstractionName: String): List[Transformation[Tree, Tree]]
  }

  type PreparationError = String

  /**
   * Transforms a selection into a list of possible extractions.
   */
  trait ExtractionCollector[E <: Extraction] {
    /**
     * Collects a list of extractions that are applicable for selection `s`
     * If no extraction is applicable an appropriate error message is returned.
     */
    def collectExtractions(s: Selection): Either[PreparationError, List[E]] = {
      def inner(validSel: Selection) = {
        val vs = VisibilityScope(validSel)
        vs.flatMap { scope =>
          prepareExtraction(validSel, scope)
        }.toList match {
          case Nil => Left(noExtractionMsg)
          case es => Right(es)
        }
      }

      for {
        validSelection <- prepareExtractionSource(s).right
        extractions <- inner(validSelection).right
      } yield extractions
    }
    
    val noExtractionMsg = "No insertion position found."
    
    /**
     * Expands the selection `s` if necessary or returns an error message if no
     * extraction is not applicable.
     */
    def prepareExtractionSource(s: Selection): Either[PreparationError, Selection]

    /**
     * Returns a list of possible extractions applicable to the visibility scope
     * `scope`.
     */
    def prepareExtraction(s: Selection, scope: VisibilityScope): List[E]
  }
}