package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.ImportAnalysis

abstract class ExtractParameter extends ExtractionRefactoring with ParameterExtractions

trait ParameterExtractions extends Extractions with ImportAnalysis {

  override def prepareExtractionSource(s: Selection) = {
    findExtractionSource(s.expand) { s =>
      s.representsValue && !s.representsParameter
    }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
  }

  override def prepareInsertionPosition(s: Selection) =
    atEndOfValueParameterList

  override def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) = {
    Right(targets.map(ParameterExtraction(source, _)))
  }

  case class ParameterExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "") extends Extraction {

    val name = ???

    def perform() = ???

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }
}