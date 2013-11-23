package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.MultiStageRefactoring

trait ExtractionRefactoring extends MultiStageRefactoring with Extractions with CompilerAccess {
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