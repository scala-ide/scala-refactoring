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

  case class ValueExtraction(extractionSource: Selection, extractionTarget: ExtractionTarget) extends Extraction {
    val name = extractionTarget.enclosing match {
      case t: Template => s"Extract Value to ${t.symbol.owner.decodedName}"
      case _ => "Extract Local Value"
    }

    def perform(abstractionName: String) = {
      val outboundDeps = extractionSource.outboundLocalDeps
      val call = mkCallValDef(name, outboundDeps)

      val returnStatements =
        if (outboundDeps.isEmpty) Nil
        else mkReturn(outboundDeps) :: Nil

      val statements = extractionSource.selectedTopLevelTrees ::: returnStatements

      val abstraction = statements match {
        case expr :: Nil => mkValDef(abstractionName, expr)
        case stmts => mkValDef(abstractionName, mkBlock(stmts))
      }

      extractionSource.replaceBy(call, preserveHierarchy = true) ::
        extractionTarget.insert(abstraction) ::
        Nil
    }
  }

  object ValueExtraction extends ExtractionCollector[ValueExtraction] {
    def prepareExtractionSource(s: Selection) = {
      findExtractionSource(s.expand) { s =>
        (s.representsValue || s.representsValueDefinitions) && !s.representsParameter
      }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
    }

    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) =
      validTargets(source, targets) match {
        case Nil => Left(noExtractionMsg)
        case ts => Right(ts.map(t => ValueExtraction(source, t)))
      }

    def validTargets(source: Selection, targets: List[ExtractionTarget]) = targets.takeWhile { t =>
      source.inboundLocalDeps.forall(t.scope.sees(_))
    }
  }
}