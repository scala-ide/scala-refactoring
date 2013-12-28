package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions{
  val collector = AutoExtraction
}

trait AutoExtractions extends MethodExtractions with ValueExtractions {
  object AutoExtraction extends ExtractionCollector[Extraction] {
    def isValidExtractionSource(s: Selection) =
      MethodExtraction.isValidExtractionSource(s)

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val valueExtractions =
        if (source.mayHaveSideEffects)
          Nil
        else
          ValueExtraction.createExtractions(source, targets)

      val remainingTargets = targets.filterNot(t => valueExtractions.exists(e => e.extractionTarget == t))

      val methodExtractions = MethodExtraction.createExtractions(source, remainingTargets)

      valueExtractions ::: methodExtractions
    }
  }
}