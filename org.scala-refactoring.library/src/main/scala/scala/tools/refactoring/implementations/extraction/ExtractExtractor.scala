package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

abstract class ExtractExtractor extends ExtractionRefactoring with ExtractorExtractions

trait ExtractorExtractions extends Extractions {
  import global._

  def prepareExtractionSource(s: Selection) = {
    findExtractionSource(s) { s =>
      s.selectedTopLevelTrees match {
        case (_: CaseDef | ExtractablePattern(_)) :: Nil =>
          s.findSelectedOfType[CaseDef].isDefined
        case _ => false
      }
    }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
  }

  override def prepareInsertionPosition(s: Selection) =
    s.beforeSelectionInBlock orElse
      s.afterSelectionInTemplate orElse
      atBeginningOfNewDefBody orElse
      atBeginningOfNewFunctionBody

  def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) = {
    val validTargets = targets.takeWhile { t =>
      source.inboundDeps.forall(t.scope.sees(_))
    }

    validTargets match {
      case Nil => Left(noExtractionMsg)
      case ts => source.selectedTopLevelTrees.head match {
        case cd: CaseDef => Right(ts.map(t => CasePatternExtraction(cd, source, t)))
        case pat => Right(ts.map(t => PatternExtraction(pat, source, t)))
      }
    }
  }

  object ExtractablePattern {
    def unapply(t: Tree): Option[Tree] = t match {
      case _: Star => None
      case t => Some(t)
    }
  }

  /**
   * Extracts an extractor based on a CaseDef. Reuses its pattern and guard.
   */
  case class CasePatternExtraction(
    caseDef: CaseDef,
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "Extracted") extends ExtractorExtraction {

    val extractorDef = {
      mkExtractor {
        val cases =
          caseDef.copy(body = matchedResult) ::
            CaseDef(Ident(nme.WILDCARD), EmptyTree, notMatchedResult) ::
            Nil

        Match(Ident(newTermName(unapplyParamName)), cases)
      }
    }

    val extractorCall = {
      caseDef.copy(pat = mkUnapplyCall(), guard = EmptyTree)
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }

  /**
   * Extracts an extractor based on a pattern.
   */
  case class PatternExtraction(
    pattern: Tree,
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "Extracted") extends ExtractorExtraction {

    val extractorDef = {
      mkExtractor {
        val patternFixedPos = pattern match{
          case t: Apply => t
          case t => t.duplicate.setPos(NoPosition)
        }

        val cases =
          CaseDef(patternFixedPos, EmptyTree, matchedResult) ::
            CaseDef(Ident(nme.WILDCARD), EmptyTree, notMatchedResult) ::
            Nil

        Match(Ident(newTermName("x")), cases)
      }
    }

    val extractorCall =
      mkUnapplyCall()

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }

  trait ExtractorExtraction extends Extraction {

    val extractorDef: Tree

    val extractorCall: Tree

    val unapplyParamName = "x"

    def perform() = {
      extractionSource.replaceBy(extractorCall) ::
        extractionTarget.insert(extractorDef) ::
        Nil
    }

    val displayName = extractionTarget.enclosing match {
      case t: Template => s"Extract Extractor to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Extractor"
    }

    val matchedTpe =
      extractionSource.findSelectedOfType[Match].get.selector.tpe

    val bindings = extractionSource.selectedTopLevelTrees.head.collect {
      case t: Bind => t
    }

    val boundNames = bindings.map(_.nameString).distinct

    val matchedResult = {
      PlainText.Raw(s"Some(${boundNames.mkString(", ")})")
    }

    val notMatchedResult = {
      PlainText.Raw(s"None")
    }

    def mkExtractor(unapplyBody: Tree) = {
      val param = mkParam(unapplyParamName, matchedTpe)

      val unapplyDef = DefDef(
        NoMods withPosition (Flags.METHOD, NoPosition),
        newTermName(nme.unapply), Nil, List(List(param)), EmptyTree, unapplyBody)

      mkModule(NoMods, abstractionName, unapplyDef :: Nil)
    }

    def mkUnapplyCall() =
      PlainText.Raw(s"${abstractionName}(${boundNames.mkString(", ")})")
  }
}