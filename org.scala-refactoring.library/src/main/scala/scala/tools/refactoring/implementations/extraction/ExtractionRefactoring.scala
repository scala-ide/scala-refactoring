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

  val collector: ExtractionCollector[_ <: Extraction]

  def prepare(s: Selection) =
    collector.collect(s)
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

  type ErrorMsg = String

  val defaultAbstractionName = "extracted"

  /**
   * A concrete and applicable extraction.
   */
  trait Extraction {
    /**
     * The code to extract
     */
    val extractionSource: Selection

    /**
     * Where the new abstraction will be inserted.
     */
    val extractionTarget: ExtractionTarget

    /**
     * Name of the new abstraction introduced by this extraction.
     */
    val abstractionName: String

    def withAbstractionName(name: String): this.type

    /**
     * A brief description of the extraction.
     */
    val displayName: String

    /**
     * Returns one or more transformations required to perform
     * the extraction.
     */
    def perform(): List[Transformation[Tree, Tree]]
  }

  /**
   * Represents a target for extractions with the according scope for
   * dependency lookups.
   */
  case class ExtractionTarget(scope: ScopeTree, enclosing: Tree, ip: InsertionPosition) {
    /**
     * Approximated position where new trees are inserted.
     */
    lazy val pos = ip(enclosing).pos

    /**
     * Inserts `t` at the targeted position.
     */
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

  trait ExtractionCollector[E <: Extraction] {
    /**
     * Expands selection `s` until it is applicable for extractions
     * of type `E`. If an appropriate selection is found, it returns
     * possible extractions for all target scopes.
     */
    def collect(s: Selection): Either[ErrorMsg, List[E]] = {
      s.expand.expandTo(isValidExtractionSource(_)).map { source =>
        val mkTarget = createExtractionTargets(createInsertionPosition(source), ScopeTree.build(source))_
        val targets = source
          .filterSelected(isValidTargetTree(source, _))
          .reverse
          .flatMap { enclosing =>
            mkTarget(enclosing)
          }

        Right(createExtractions(source, targets))
      }.getOrElse(Left("No extraction for current selection found."))
    }

    /**
     * Does `s` represent code that is extractable by extractions
     * constructed by this collector?
     */
    private[extraction] def isValidExtractionSource(s: Selection): Boolean

    /**
     * Creates extractions that extract code from `source` and inserts a
     * new abstraction in a target from `targets`.
     */
    private[extraction] def createExtractions(source: Selection, targets: List[ExtractionTarget]): List[E]

    /**
     * Is `t` a feasible target for extracted abstractions?
     */
    private[extraction] def isValidTargetTree(s: Selection, t: Tree) =
      !t.pos.sameRange(s.pos) &&
        (t match {
          // If the selection selects parts of a case pattern or guard, the body is not a feasible target
          // because it is not visible from guard or pattern
          case CaseDef(pat, guard, _) if (pat.pos union guard.pos).includes(s.pos) => false
          case _ => true
        })

    private[extraction] def createInsertionPosition(s: Selection): InsertionPosition =
      s.beforeSelectionInBlock orElse
        s.afterSelectionInTemplate orElse
        atBeginningOfNewDefBody orElse
        atBeginningOfNewFunctionBody orElse
        atBeginningOfCaseBody

    private[extraction] def createExtractionTargets(ip: InsertionPosition, scopes: ScopeTree)(t: Tree): List[ExtractionTarget] =
      if (ip.isDefinedAt(t)) {
        val scope = scopes.findScopeFor(ip(t).pos)
        ExtractionTarget(scope, t, ip) :: Nil
      } else {
        Nil
      }
  }
}