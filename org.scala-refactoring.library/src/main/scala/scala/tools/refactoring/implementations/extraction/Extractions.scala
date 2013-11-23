package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.transformation.TransformableScopes
import scala.tools.refactoring.transformation.TransformableSelections

trait Extractions extends TransformableScopes with TransformableSelections with Abstractions { self: CompilerAccess =>
  import global._

  trait Extraction {
    val scope: VisibilityScope

    val extractionSource: Selection

    val name: String

    def perform(abstractionName: String): List[Transformation[Tree, Tree]]
  }

  type PreparationError = String

  trait ExtractionCollector[E <: Extraction] {
    def prepareExtractionSource(s: Selection): Either[PreparationError, Selection]

    def prepareExtraction(s: Selection, scope: VisibilityScope): Option[E]

    def collectExtractions(s: Selection): Either[PreparationError, List[E]] = {
      def inner(validSel: Selection) = {
        val vs = VisibilityScope(validSel)
        vs.flatMap { scope =>
          prepareExtraction(validSel, scope)
        }.toList match {
          case Nil => Left("No insertion position found.")
          case es => Right(es)
        }
      }

      for {
        validSelection <- prepareExtractionSource(s).right
        extractions <- inner(validSelection).right
      } yield extractions
    }
  }

  case class ValueExtraction(extractionSource: Selection, scope: VisibilityScope) extends Extraction {
    val name = scope match {
      case t: TemplateScope => s"Extract Value to ${t.name}"
      case _ => s"Extract Local Value"
    }

    def perform(abstractionName: String) = {
      val abstr = ValueAbstraction(abstractionName, extractionSource, extractionSource.outboundLocalDeps)
      extractionSource.replaceBy(abstr.call, preserveHierarchy = true) ::
        scope.insert(abstr.abstraction) ::
        Nil
    }
  }

  object ValueExtraction extends ExtractionCollector[ValueExtraction] {
    def prepareExtractionSource(s: Selection) = {
      val expanded = s.expand
      if (expanded.representsValue || expanded.representsValueDefinitions)
        Right(expanded)
      else
        Left("Cannot extract selection")
    }

    def prepareExtraction(s: Selection, vs: VisibilityScope) = vs match {
      case _: TemplateScope | _: MethodScope | _: FunctionScope | _: BlockScope | _: CaseScope if vs.undefinedDependencies.isEmpty =>
        Some(ValueExtraction(s, vs))
      case _ => None
    }
  }

  case class MethodExtraction(extractionSource: Selection, scope: VisibilityScope) extends Extraction {
    val name = scope match {
      case t: TemplateScope => s"Extract Method to ${t.name}"
      case _ => s"Extract Local Method"
    }

    def perform(abstractionName: String) =
      perform(abstractionName, Nil)

    def perform(abstractionName: String, selectedParameters: List[Symbol]) = {
      val abstr = MethodAbstraction(
        abstractionName, extractionSource,
        scope.undefinedDependencies union selectedParameters,
        extractionSource.outboundLocalDeps)

      extractionSource.replaceBy(abstr.call, preserveHierarchy = true) ::
        scope.insert(abstr.abstraction) ::
        Nil
    }
  }

  object MethodExtraction extends ExtractionCollector[MethodExtraction] {
    def prepareExtractionSource(s: Selection) = {
      val expanded = s.expand
      if (expanded.representsValue || expanded.representsValueDefinitions)
        Right(expanded)
      else
        Left("Cannot extract selection")
    }

    def prepareExtraction(s: Selection, vs: VisibilityScope) = vs match {
      case _: TemplateScope | _: MethodScope | _: FunctionScope | _: BlockScope | _: CaseScope =>
        Some(MethodExtraction(s, vs))
      case _ => None
    }
  }

  object AutoExtraction extends ExtractionCollector[Extraction] {
    def prepareExtractionSource(s: Selection) =
      MethodExtraction.prepareExtractionSource(s)

    def prepareExtraction(s: Selection, vs: VisibilityScope) = vs match {
      case _: TemplateScope | _: MethodScope | _: FunctionScope | _: BlockScope | _: CaseScope =>
        if (vs.undefinedDependencies.isEmpty && !vs.referenceSelection.mayHaveSideEffects) {
          Some(ValueExtraction(s, vs))
        } else {
          Some(MethodExtraction(s, vs))
        }
      case _ => None
    }
  }
}