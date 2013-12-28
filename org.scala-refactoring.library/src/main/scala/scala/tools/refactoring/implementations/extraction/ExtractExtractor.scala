package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

abstract class ExtractExtractor extends ExtractionRefactoring with ExtractorExtractions {
  val collector = ExtractorExtraction
}

trait ExtractorExtractions extends Extractions {
  import global._

  object ExtractorExtraction extends ExtractionCollector[ExtractorExtraction] {
    def isValidExtractionSource(s: Selection) = {
      s.selectedTopLevelTrees match {
        case (cd @ CaseDef(_, _, body)) :: Nil =>
          !body.pos.includes(s.pos)
        case ExtractablePattern(_) :: Nil =>
          s.findSelectedOfType[CaseDef].map{ cd =>
            !cd.body.pos.includes(s.pos)
          }.getOrElse(false)
        case _ => false
      }
    }

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val validTargets = targets.takeWhile { t =>
        source.inboundDeps.forall(t.scope.sees(_))
      }

      source.selectedTopLevelTrees.head match {
        case cd: CaseDef => validTargets.map(CasePatternExtraction(cd, source, _))
        case pat => validTargets.map(PatternExtraction(pat, source, _))
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
          val patternFixedPos = pattern match {
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

      extractionSource.replaceBy(extractorCall) ::
        extractionTarget.insert(extractorDef) ::
        Nil
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }

  trait ExtractorExtraction extends Extraction {
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
        newTermName(nme.unapply), Nil, List(List(param)), EmptyTree, unapplyBody)

      mkModule(NoMods, abstractionName, unapplyDef :: Nil)
    }

    def mkUnapplyCall() =
      PlainText.Raw(s"${abstractionName}(${boundNames.mkString(", ")})")
  }
}