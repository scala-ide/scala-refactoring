package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.ImportAnalysis
import scala.tools.refactoring.analysis.Indexes

abstract class ExtractParameter extends ExtractionRefactoring with ParameterExtractions

/**
 * Extracts a parameter
 */
trait ParameterExtractions extends Extractions with ImportAnalysis {
  import global._

  override def prepareExtractionSource(s: Selection) = {
    findExtractionSource(s) { s =>
      s.representsValue && !s.representsParameter
    }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
  }

  override def prepareInsertionPosition(s: Selection) =
    atEndOfValueParameterList

  def prepareExtractions(source: Selection, targets: List[ExtractionTarget]): Either[ErrorMsg, List[Extraction]] = {
    Right(targets.map(ParameterExtraction(source, _)))
  }

  case class ParameterExtraction(
    extractionSource: Selection, 
    extractionTarget: ExtractionTarget, 
    abstractionName: String = defaultAbstractionName) extends Extraction {

    val dsiplayName = extractionTarget.enclosing match {
      case t: DefDef => s"Extract Method Parameter to ${t.symbol.nameString}"
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