package scala.tools.refactoring.implementations.extraction

abstract class ExtractCode extends ExtractionRefactoring with AutoExtractions {
  import global._

  type E = Extraction

  val collector = AutoExtraction

  case class RefactoringParameters(
    selectedExtraction: E,
    name: String)

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) =
    perform(params.selectedExtraction, params.name)
}

trait AutoExtractions extends MethodExtractions with ValueExtractions {
  object AutoExtraction extends ExtractionCollector[Extraction] {
    def prepareExtractionSource(s: Selection) =
      MethodExtraction.prepareExtractionSource(s)

    def prepareExtraction(s: Selection, vs: VisibilityScope) =
      if (vs.undefinedDependencies.isEmpty && !vs.referenceSelection.mayHaveSideEffects) {
        ValueExtraction.prepareExtraction(s, vs)
      } else {
        MethodExtraction.prepareExtraction(s, vs)
      }
  }
}