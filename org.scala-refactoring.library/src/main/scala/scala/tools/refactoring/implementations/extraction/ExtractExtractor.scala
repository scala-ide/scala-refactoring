package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

abstract class ExtractExtractor extends ExtractionRefactoring with ExtractorExtractions

trait ExtractorExtractions extends Extractions {
  import global._

  object Pattern {
    def unapply(t: Tree): Option[Tree] = t match {
      case t @ (_: Bind | _: Alternative | _: Star | _: UnApply) => Some(t)
      case _ => None
    }
  }

  def prepareExtractionSource(s: Selection) = {
    findExtractionSource(s) { s =>
      s.selectedTopLevelTrees.map { t =>
        println(rootMirror.universe.showRaw(t))
      }
      s.selectedTopLevelTrees match {
        case (_: CaseDef | Pattern(_)) :: Nil => true
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
        case _: CaseDef => Right(ts.map(t => ExtractorExtraction(source, t)))
        case _ => Right(ts.map(t => ExtractorExtraction(source, t)))
      }
    }
  }

  case class ExtractorExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "Extracted") extends Extraction {

    val displayName = extractionTarget.enclosing match {
      case t: Template => s"Extract Extractor to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Extractor"
    }

    val patternOrCase = extractionSource.selectedTopLevelTrees.head

    val bindings = patternOrCase.collect {
      case t: Bind => t
    }
    
    val boundNames = bindings.map(_.nameString).distinct
    
    val matchedResult = {
      Apply(Select(Select(Ident("scala"), "Some"), "apply"), List(Ident(boundNames.head)))
    }
    
    val notMatchedResult = {
      Select(Ident("scala"), "None")
    }

    val matchedTpe = {
      val matchStmt = extractionSource.findSelectedOfType[Match].get
      matchStmt.selector.tpe
    }

    val resultTpe = {
      ()
    }

    def perform() = {
      val param = mkParam("x", matchedTpe)

      val cases = (patternOrCase match {
        case CaseDef(pat, guard, body) => CaseDef(pat, guard, matchedResult)
        case pat => CaseDef(pat, EmptyTree, matchedResult)
      }) :: CaseDef(Ident(nme.WILDCARD), EmptyTree, notMatchedResult) :: Nil

      val matchTree = Match(Ident(newTermName("x")), cases)

      val unapplyDef = DefDef(
        NoMods withPosition (Flags.METHOD, NoPosition),
        newTermName("unapply"), Nil, List(List(param)), EmptyTree, matchTree)

      val extractor = mkModule(NoMods, abstractionName, unapplyDef :: Nil)

      extractionTarget.insert(extractor) ::
        Nil
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }
}