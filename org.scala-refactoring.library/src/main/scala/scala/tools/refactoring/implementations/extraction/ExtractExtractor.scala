package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

/**
 * Extracts patterns in case statements to new extractor objects.
 */
abstract class ExtractExtractor extends ExtractionRefactoring with ExtractorExtractions {
  val collector = ExtractorExtraction
}

trait ExtractorExtractions extends Extractions {
  import global._

  object ExtractorExtraction extends ExtractionCollector[ExtractorExtraction] {
    def isValidExtractionSource(s: Selection) = {
      s.selectedTopLevelTrees match {
        case (cd: CaseDef) :: Nil =>
          true
        case (_: Star) :: Nil =>
          // Star patterns are only extractable as parts of other patterns
          false
        case _ :: Nil =>
          // Trees that are part of a pattern in a CaseDef
          // must be patterns and are therefore extractable
          s.findSelectedOfType[CaseDef].map { cd =>
            cd.pat.pos.includes(s.pos)
          }.getOrElse(false)
        case _ =>
          false
      }
    }

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val validTargets = targets.takeWhile { t =>
        source.inboundDeps.forall(t.scope.sees(_))
      }

      source.selectedTopLevelTrees.head match {
        case cd: CaseDef => 
          validTargets.map(CasePatternExtraction(cd, source, _))
        case pat => 
          validTargets.map(PatternExtraction(pat, source, _))
      }
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

    def perform() = {
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

      extractionSource.replaceBy(extractorCall) ::
        extractionTarget.insert(extractorDef) ::
        Nil
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

    def perform() = {
      val extractorDef = {
        mkExtractor {
          /* Remove pattern position in some cases in order
           * to reprint the tree with pretty printer.
           */
          val patternFixedPos = pattern match {
            case t @ (_: Apply | _: UnApply) => t
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

      extractionSource.replaceBy(extractorCall) ::
        extractionTarget.insert(extractorDef) ::
        Nil
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }

  /**
   * Base trait of extractor extractions.
   * Allows to create different extractors based on the
   * selected pattern.
   */
  sealed trait ExtractorExtraction extends Extraction {
    val unapplyParamName = "x"

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
        nme.unapply, Nil, List(List(param)), EmptyTree, unapplyBody)

      mkModule(NoMods, abstractionName, unapplyDef :: Nil)
    }

    def mkUnapplyCall() =
      PlainText.Raw(s"${abstractionName}(${boundNames.mkString(", ")})")
  }
}