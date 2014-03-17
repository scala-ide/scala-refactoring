package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.analysis.ImportAnalysis

/**
 * Extracts one or more expressions into a new val definition.
 */
abstract class ExtractValue extends ExtractionRefactoring with ValueExtractions {
  val collector = ValueExtraction
}

trait ValueExtractions extends Extractions with ImportAnalysis {
  import global._

  object ValueExtraction extends ExtractionCollector[ValueExtraction] {
    def isValidExtractionSource(s: Selection) =
      (s.representsValue || s.representsValueDefinitions) && !s.representsParameter

    def createExtractions(source: Selection, targets: List[ExtractionTarget]) = {
      val validTargets = targets.takeWhile { t =>
        source.inboundLocalDeps.forall(t.scope.sees(_))
      }

      validTargets.map(ValueExtraction(source, _))
    }
  }

  case class ValueExtraction(
    extractionSource: Selection,
    extractionTarget: ExtractionTarget,
    abstractionName: String = defaultAbstractionName) extends Extraction {

    val displayName = extractionTarget.enclosing match {
      case t: Template => s"Extract Value to ${t.symbol.owner.decodedName}"
      case _ => "Extract Local Value"
    }

    def withAbstractionName(name: String) =
      copy(abstractionName = name).asInstanceOf[this.type]

    lazy val imports = buildImportTree(extractionSource.root)

    def perform() = {
      val outboundDeps = extractionSource.outboundLocalDeps
      val call = mkCallValDef(abstractionName, outboundDeps)

      val returnStatements =
        if (outboundDeps.isEmpty) Nil
        else mkReturn(outboundDeps) :: Nil

      val importStatements = extractionSource.selectedTopLevelTrees.flatMap(imports.findRequiredImports(_, extractionSource.pos, extractionTarget.pos))

      val extractedStatements = extractionSource.selectedTopLevelTrees match {
        case (fn @ Function(vparams, Block(stmts, expr))) :: Nil =>
          val tpe = fn.tpe
          val newFn = fn copy (body = Block(stmts, expr))
          newFn.tpe = tpe
          newFn :: Nil
        case ts => ts
      }

      val statements = importStatements ::: extractedStatements ::: returnStatements

      val abstraction = statements match {
        case expr :: Nil =>
          expr match {
            // Add explicit type annotations for extracted functions
            case fn: Function =>
              mkValDef(abstractionName, fn, TypeTree(fn.tpe))
            case t =>
              mkValDef(abstractionName, t)
          }
        case stmts => mkValDef(abstractionName, mkBlock(stmts))
      }

      extractionSource.replaceBy(call, preserveHierarchy = true) ::
        extractionTarget.insert(abstraction) ::
        Nil
    }
  }
}