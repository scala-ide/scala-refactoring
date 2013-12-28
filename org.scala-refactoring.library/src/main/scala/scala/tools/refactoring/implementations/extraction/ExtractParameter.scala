package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.ImportAnalysis
import scala.tools.refactoring.analysis.Indexes

abstract class ExtractParameter extends ExtractionRefactoring with ParameterExtractions {
  val collector = ParameterExtraction
}

/**
 * Extracts an expression into a new parameter whose default value is 
 * the extracted expression.
 */
trait ParameterExtractions extends Extractions with ImportAnalysis {
  import global._

  object ParameterExtraction extends ExtractionCollector[ParameterExtraction] {
    def isValidExtractionSource(s: Selection) =
      s.representsValue && !s.representsParameter

    override def createInsertionPosition(s: Selection) = 
      atEndOfValueParameterList

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val validTargets = targets.takeWhile { t =>
        source.inboundLocalDeps.forall(t.scope.sees(_))
      }
      
      validTargets.map(ParameterExtraction(source, _))
    }
  }

  case class ParameterExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = defaultAbstractionName) extends Extraction {

    val displayName = extractionTarget.enclosing match {
      case t: DefDef => s"Extract Parameter to Method ${t.symbol.nameString}"
    }

    val functionOrDefDef = extractionTarget.enclosing

    def perform() = {
      val tpe = defaultVal.tpe
      val param = mkParam(abstractionName, tpe, defaultVal)
      val paramRef = Ident(newTermName(abstractionName))

      extractionSource.replaceBy(paramRef) ::
        extractionTarget.insert(param) ::
        Nil
    }

    val defaultVal = {
      extractionSource.selectedTopLevelTrees.last
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }
}