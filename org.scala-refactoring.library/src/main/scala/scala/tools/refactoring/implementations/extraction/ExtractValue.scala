package scala.tools.refactoring.implementations.extraction

/**
 * Extracts one or more expressions into a new val definition.
 */
trait ExtractValue extends ExtractionRefactoring with ValueExtractions {
  import global._

  type E = ValueExtraction

  val collector = ValueExtraction

  case class RefactoringParameters(
    selectedExtraction: E,
    name: String)

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) =
    perform(params.selectedExtraction, params.name)
}

trait ValueExtractions extends Extractions {
  import global._

  case class ValueExtraction(extractionSource: Selection, scope: VisibilityScope) extends Extraction {
    val name = scope match {
      case t: TemplateScope => s"Extract Value to ${t.name}"
      case _ => s"Extract Local Value"
    }

    def perform(abstractionName: String) = {
      val abstr = ValueAbstraction(abstractionName, extractionSource, extractionSource.outboundLocalDeps)
      extractionSource.replaceBy(abstr.call, preserveHierarchy = true) ::
        scope.insert(abstr.abstraction) ::
        Nil
    }
  }

  object ValueExtraction extends ExtractionCollector[ValueExtraction] {
    def prepareExtractionSource(s: Selection) = {
      val expanded = s.expand
      if (expanded.representsValue || expanded.representsValueDefinitions)
        Right(expanded)
      else
        Left("Cannot extract selection")
    }

    def prepareExtraction(s: Selection, vs: VisibilityScope) = vs match {
      case _: TemplateScope | _: MethodScope | _: FunctionScope | _: BlockScope | _: CaseScope if vs.undefinedDependencies.isEmpty =>
        ValueExtraction(s, vs) :: Nil
      case ms: MethodScope if !ms.enclosing.rhs.isInstanceOf[Block] =>
        ValueExtraction(s, vs) :: Nil
      case _ => Nil
    }
  }
}