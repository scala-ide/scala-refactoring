package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags

abstract class ExtractMethod extends ExtractionRefactoring with MethodExtractions {
  import global._

  type E = MethodExtraction

  val collector = MethodExtraction

  type RefactoringParameters = E

  def perform(s: Selection, prepared: PreparationResult, extraction: RefactoringParameters) =
    perform(extraction)
}

trait MethodExtractions extends Extractions {
  import global._

  case class MethodExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = "",
    selectedParameters: List[Symbol] = Nil) extends Extraction {

    val name = extractionTarget.enclosing match {
      case t: Template => s"Extract Method to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Method"
    }

    lazy val requiredParameters =
      extractionSource.inboundLocalDeps.filterNot { dep =>
        extractionTarget.scope.sees(dep)
      }

    lazy val optionalParameters =
      extractionSource.inboundLocalDeps diff requiredParameters

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]

    def withSelectedParameters(params: List[Symbol]) =
      copy(selectedParameters = params)

    def perform() = {
      val parameters = requiredParameters union selectedParameters
      val outboundDeps = extractionSource.outboundLocalDeps

      val call = mkCallDefDef(abstractionName, parameters :: Nil, outboundDeps)

      val returnStatements =
        if (outboundDeps.isEmpty) Nil
        else mkReturn(outboundDeps) :: Nil

      val statements = extractionSource.selectedTopLevelTrees ::: returnStatements

      val abstraction = {
        /* We implement a simpler version of mkDefDef in order to address
         * issues with symbols that are treated as by name parameters
         */
        def symbolToParam(s: Symbol) = {
          /* The type of a symbol referencing class fields is "=> T"
           * and therefore converted to a by name parameter. But in most cases
           * it is preferred to pass it by value.
           */
          val tpe = if (s.tpe.toString.startsWith("=>"))
            s.tpe.baseTypeSeq(0)
          else
            s.tpe
          new ValDef(Modifiers(Flags.PARAM), newTermName(s.nameString), TypeTree(tpe), EmptyTree)
        }

        val ps = parameters.map(symbolToParam) :: Nil

        val returnTpe = statements.last match {
          case t: Function if t.pos.isTransparent =>
            TypeTree(t.body.tpe)
          case t =>
            TypeTree(t.tpe)
        }

        DefDef(NoMods withPosition (Flags.METHOD, NoPosition), newTermName(abstractionName), Nil, ps, returnTpe, mkBlock(statements))
      }

      extractionSource.replaceBy(call, preserveHierarchy = true) ::
        extractionTarget.insert(abstraction) ::
        Nil
    }
  }

  object MethodExtraction extends ExtractionCollector[MethodExtraction] {
    def prepareExtractionSource(s: Selection) = {
      findExtractionSource(s.expand) { s =>
        (s.representsValue || s.representsValueDefinitions) && !s.representsParameter
      }.map(Right(_)).getOrElse(Left("Cannot extract selection"))
    }

    def prepareExtractions(source: Selection, targets: List[ExtractionTarget]) =
      validTargets(source, targets) match {
        case Nil => Left(noExtractionMsg)
        case ts => Right(ts.map(t => MethodExtraction(source, t)))
      }

    def validTargets(source: Selection, targets: List[ExtractionTarget]) = {
      targets.takeWhile { t =>
        source.inboundLocalDeps.forall(dep => t.scope.sees(dep) || isAllowedAsParameter(dep))
      }
    }
  }

  def isAllowedAsParameter(s: Symbol) =
    s.isValue &&
      !s.name.isOperatorName &&
      !s.isImplicit
}