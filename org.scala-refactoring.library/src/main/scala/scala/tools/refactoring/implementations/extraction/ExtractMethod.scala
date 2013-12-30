package scala.tools.refactoring.implementations.extraction

import scala.reflect.internal.Flags
import scala.tools.refactoring.analysis.ImportAnalysis

abstract class ExtractMethod extends ExtractionRefactoring with MethodExtractions {
  val collector = MethodExtraction
}

/**
 * Extracts one or more expressions into a new method. Each inbound dependency
 * to the extracted code that is not accessible from the target scope becomes
 * a parameter to the new method.
 */
trait MethodExtractions extends Extractions with ImportAnalysis {
  import global._

  object MethodExtraction extends ExtractionCollector[MethodExtraction] {
    def isValidExtractionSource(s: Selection) =
      (s.representsValue || s.representsValueDefinitions) && !s.representsParameter

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val validTargets = targets.takeWhile { t =>
        source.inboundLocalDeps.forall(dep => t.scope.sees(dep) || isAllowedAsParameter(dep))
      }

      validTargets.map(MethodExtraction(source, _))
    }

    def isAllowedAsParameter(s: Symbol) =
      s.isValue && (s.isVal || s.isAccessor)
  }

  case class MethodExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = defaultAbstractionName) extends Extraction {

    val displayName = extractionTarget.enclosing match {
      case t: Template => s"Extract Method to ${t.symbol.owner.decodedName}"
      case _ => s"Extract Local Method"
    }

    /**
     * Inbound dependencies not defined in the target scope and therefore
     * must become parameters to the new method.
     */
    lazy val parameters =
      extractionSource.inboundLocalDeps.filterNot { dep =>
        extractionTarget.scope.sees(dep)
      }

    lazy val imports = buildImportTree(extractionSource.root)

    def perform() = {
      val outboundDeps = extractionSource.outboundLocalDeps

      val call = mkCallDefDef(abstractionName, parameters :: Nil, outboundDeps)

      val returnStatements =
        if (outboundDeps.isEmpty) Nil
        else mkReturn(outboundDeps) :: Nil

      val importStatements = extractionSource.selectedTopLevelTrees.flatMap(imports.findRequiredImports(_, extractionSource.pos, extractionTarget.pos))

      val statements = importStatements ::: extractionSource.selectedTopLevelTrees ::: returnStatements

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
          ValDef(Modifiers(Flags.PARAM), newTermName(s.nameString), TypeTree(tpe), EmptyTree)
        }

        val ps = parameters.map(symbolToParam) :: Nil

        DefDef(NoMods withPosition (Flags.METHOD, NoPosition), newTermName(abstractionName), Nil, ps, EmptyTree, mkBlock(statements))
      }

      extractionSource.replaceBy(call, preserveHierarchy = true) ::
        extractionTarget.insert(abstraction) ::
        Nil
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]
  }
}