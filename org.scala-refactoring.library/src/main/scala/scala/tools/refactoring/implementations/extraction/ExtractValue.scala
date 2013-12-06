package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.ImportAnalysis

/**
 * Extracts one or more expressions into a new val definition.
 */
trait ExtractValue extends ExtractionRefactoring with ValueExtractions {
  import global._

  type E = ValueExtraction

  val collector = ValueExtraction

  type RefactoringParameters = E

  def perform(s: Selection, prepared: PreparationResult, extraction: RefactoringParameters) =
    perform(extraction)
}

trait ValueExtractions extends Extractions with ImportAnalysis {
  import global._

  case class ValueExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    imports: ImportTree,
    abstractionName: String = "") extends Extraction {

    val name = extractionTarget.enclosing match {
      case t: Template => s"Extract Value to ${t.symbol.owner.decodedName}"
      case _ => "Extract Local Value"
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]

    def perform() = {
      val outboundDeps = extractionSource.outboundLocalDeps
      val call = mkCallValDef(abstractionName, outboundDeps)

      val returnStatements =
        if (outboundDeps.isEmpty) Nil
        else mkReturn(outboundDeps) :: Nil

      val importStatements = extractionSource.selectedTopLevelTrees.flatMap(imports.findRequiredImports(_, extractionSource.pos, extractionTarget.enclosing.pos)).distinct

      val statements = importStatements ::: extractionSource.selectedTopLevelTrees ::: returnStatements

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

    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val imports = buildImportTree(source.root)

      validTargets(source, targets) match {
        case Nil => Left(noExtractionMsg)
        case ts => Right(ts.map(t => ValueExtraction(source, t, imports)))
      }
    }

    def validTargets(source: Selection, targets: List[ExtractionTarget]) = targets.takeWhile { t =>
      source.inboundLocalDeps.forall(t.scope.sees(_))
    }
  }
}