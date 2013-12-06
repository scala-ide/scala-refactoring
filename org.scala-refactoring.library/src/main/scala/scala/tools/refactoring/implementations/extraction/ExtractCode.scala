package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions {
  import global._

  type E = Extraction

  val collector = AutoExtraction

  type RefactoringParameters = E

  def perform(s: Selection, prepared: PreparationResult, extraction: RefactoringParameters) =
    perform(extraction)
}

trait AutoExtractions extends MethodExtractions with ValueExtractions {
  object AutoExtraction extends ExtractionCollector[Extraction] {
    def prepareExtractionSource(s: Selection) =
      MethodExtraction.prepareExtractionSource(s)

    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val valueTargets =
        if (source.mayHaveSideEffects) Nil
        else ValueExtraction.validTargets(source, targets)
      val methodTargets =
        MethodExtraction.validTargets(source, targets diff valueTargets)
        
      val imports = buildImportTree(source.root)

      if (valueTargets.isEmpty && methodTargets.isEmpty)
        Left(noExtractionMsg)
      else
        Right(
          valueTargets.map(ValueExtraction(source, _, imports)) :::
            methodTargets.map(MethodExtraction(source, _, imports)))
    }

  }
}