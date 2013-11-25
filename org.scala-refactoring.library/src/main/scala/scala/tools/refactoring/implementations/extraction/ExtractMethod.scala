package scala.tools.refactoring.implementations.extraction

abstract class ExtractMethod extends ExtractionRefactoring with MethodExtractions {
  import global._

  type E = MethodExtraction

  val collector = MethodExtraction

  case class RefactoringParameters(selectedExtraction: E, name: String, selectedParameters: List[Symbol])

  def perform(s: Selection, prepared: PreparationResult, params: RefactoringParameters) = {
    val transformations = params.selectedExtraction.perform(params.name, params.selectedParameters)
    Right(transformFile(s.file, transformations))
  }
}

trait MethodExtractions extends Extractions {
  import global._

  case class MethodExtraction(extractionSource: Selection, scope: VisibilityScope) extends Extraction {
    val name = scope match {
      case t: TemplateScope => s"Extract Method to ${t.name}"
      case _ => s"Extract Local Method"
    }

    def perform(abstractionName: String) =
      perform(abstractionName, Nil)

    def perform(abstractionName: String, selectedParameters: List[Symbol]) = {
      val abstr = MethodAbstraction(
        abstractionName, extractionSource,
        scope.undefinedDependencies union selectedParameters,
        extractionSource.outboundLocalDeps)

      extractionSource.replaceBy(abstr.call, preserveHierarchy = true) ::
        scope.insert(abstr.abstraction) ::
        Nil
    }
    
    lazy val requiredParameters =
      scope.undefinedDependencies

    lazy val optionalParameters =
      scope.definedDependencies.filter(isAllowedAsParameter(_))
  }

  object MethodExtraction extends ExtractionCollector[MethodExtraction] {
    def prepareExtractionSource(s: Selection) = {
      val expanded = s.expand
      if (expanded.representsValue || expanded.representsValueDefinitions)
        Right(expanded)
      else
        Left("Cannot extract selection")
    }

    def prepareExtraction(s: Selection, vs: VisibilityScope) =
      if (vs.undefinedDependencies.forall(isAllowedAsParameter(_)))
        vs match {
          case _: TemplateScope | _: FunctionScope | _: BlockScope | _: CaseScope =>
            MethodExtraction(s, vs) :: Nil
          case ms: MethodScope if !ms.enclosing.rhs.isInstanceOf[Block] =>
            MethodExtraction(s, vs) :: Nil
          case _ => Nil
        }
      else
        Nil
  }

  def isAllowedAsParameter(s: Symbol) =
    !s.name.isOperatorName &&
      !s.isImplicit
}