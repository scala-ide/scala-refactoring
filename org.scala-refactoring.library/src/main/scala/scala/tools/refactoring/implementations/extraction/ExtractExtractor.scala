package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

abstract class ExtractExtractor extends ExtractionRefactoring with ExtractorExtractions

trait ExtractorExtractions extends Extractions {
  import global._

  object ExtractablePattern {
    def unapply(t: Tree): Option[Tree] = t match {
      case t @ (_: Bind | _: Alternative | _: UnApply) =>
        Some(t)
      case t @ (_: Literal | _: Typed) =>
        Some(t)
      case t: RefTree if isConstantPatternName(t.nameString) =>
        Some(t)
      case t: Apply if t.fun.symbol.isCaseApplyOrUnapply =>
        Some(t)
      case _ => None
    }

    private def isConstantPatternName(s: String) =
      s.head >= 'A' && s.head <= 'Z'
  }

  def prepareExtractionSource(s: Selection) = {
    findExtractionSource(s) { s =>
      s.selectedTopLevelTrees match {
        case (_: CaseDef | ExtractablePattern(_)) :: Nil => true
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

  trait ExtractorExtraction extends Extraction {

    val extractorDef: Tree

    val extractorCall: Tree

    def perform() = {
      extractionSource.replaceBy(extractorCall) ::
        extractionTarget.insert(extractorDef) ::
        Nil
    }

    val unapplyParamName = "x"

    val displayName = extractionTarget.enclosing match {
      case t: Template => s"Extract Extractor to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Extractor"
    }

    val enclosingMatch = extractionSource.findSelectedOfType[Match].get

    val matchedTpe =
      enclosingMatch.selector.tpe

    val patternOrCase = extractionSource.selectedTopLevelTrees.head

    val bindings = patternOrCase.collect {
      case t: Bind => t
    }

    val boundNames = bindings.map(_.nameString).distinct

    val matchedResult = {
      Apply(Select(Select(Ident(nme.scala_), nme.Some), nme.apply), boundNames.map(Ident(_)))
    }

    val notMatchedResult = {
      Select(Ident(nme.scala_), "None")
    }

    def mkExtractor(matchTree: Tree) = {
      val param = mkParam(unapplyParamName, matchedTpe)

      val unapplyDef = DefDef(
        NoMods withPosition (Flags.METHOD, NoPosition),
        newTermName(nme.unapply), Nil, List(List(param)), EmptyTree, matchTree)

      mkModule(NoMods, abstractionName, unapplyDef :: Nil)
    }

    def mkUnapply() =
      PlainText.Raw(s"${abstractionName}(${boundNames.mkString(", ")})")
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

        Match(Ident(newTermName("x")), cases)
      }
    }

    val extractorCall = {
      caseDef.copy(pat = mkUnapply(), guard = EmptyTree)
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }

  /**
   * Extracts an extractor based on a sub pattern.
   */
  case class PatternExtraction(
    pattern: Tree,
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "Extracted") extends ExtractorExtraction {

    val extractorDef = {
      mkExtractor {
        val cases =
          CaseDef(pat = pattern, body = matchedResult) ::
            CaseDef(Ident(nme.WILDCARD), EmptyTree, notMatchedResult) ::
            Nil

        Match(Ident(newTermName("x")), cases)
      }
    }

    val extractorCall =
      mkUnapply()

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }
}