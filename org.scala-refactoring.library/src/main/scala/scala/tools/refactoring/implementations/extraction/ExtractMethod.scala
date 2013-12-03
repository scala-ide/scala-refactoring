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

  case class MethodExtraction(extractionSource: Selection, extractionTarget: ExtractionTarget) extends Extraction {
    val name = extractionTarget.enclosing match {
      case t: Template => s"Extract Method to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Method"
    }

    def perform(abstractionName: String) =
      perform(abstractionName, Nil)

    def perform(abstractionName: String, selectedParameters: List[Symbol]) = {
      val abstr = MethodAbstraction(
        abstractionName, extractionSource,
        requiredParameters union selectedParameters,
        extractionSource.outboundLocalDeps)

      extractionSource.replaceBy(abstr.call, preserveHierarchy = true) ::
        extractionTarget.insert(abstr.abstraction) ::
        Nil
    }

    lazy val requiredParameters =
      extractionSource.inboundLocalDeps.filterNot { dep =>
        extractionTarget.scope.sees(dep)
      }

    lazy val optionalParameters =
      extractionSource.inboundLocalDeps diff requiredParameters
  }

  object MethodExtraction extends ExtractionCollector[MethodExtraction] {
    def prepareExtractionSource(s: Selection) = {
      findExtractionSource(s.expand) { s =>
        (s.representsValue || s.representsValueDefinitions) && !s.representsParameter
      }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
    }
    
    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) =
      validTargets(source, targets) match {
        case Nil => Left(noExtractionMsg)
        case ts => Right(ts.map(t => MethodExtraction(source, t)))
      }

    def validTargets(source: Selection, targets: List[ExtractionTarget]) = {
      targets.takeWhile{ t =>
        source.inboundDeps.forall(dep => t.scope.sees(dep) || isAllowedAsParameter(dep))
      }
    }
  }

  def isAllowedAsParameter(s: Symbol) =
    s.isValue &&
    !s.name.isOperatorName &&
      !s.isImplicit
}