package scala.tools.refactoring.implementations

import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.{MultiStageRefactoring, analysis}

abstract class ImplementMethods extends MultiStageRefactoring with analysis.Indexes with TreeFactory with InteractiveScalaCompiler {

  import global._

  case class PreparationResult(targetTemplate: Template, methodsToImplement: Seq[DefDef])

  override def prepare(s: Selection): Either[PreparationError, PreparationResult] = {

    val methodsToImplement = for {
      selectedTemplateDeclaration <- index.declaration(s.enclosingTree.symbol).toSeq collect {
        case traitDeclaration: ClassDef => traitDeclaration
      }
      unimplementedMethod <- selectedTemplateDeclaration.impl.body collect {
        case methodDeclaration: DefDef if methodDeclaration.rhs.isEmpty => methodDeclaration
      }
    } yield unimplementedMethod

    val targetTemplate = s.expandToNextEnclosingTree.flatMap {
      _.selectedSymbolTree collect {
        case target: Template => target
      }
    }

    if(targetTemplate.isEmpty) Left {
      PreparationError("No target class in selection")
    }
    else if(methodsToImplement.isEmpty) Left {
      PreparationError("There are not methods to implement")
    }
    else Right(PreparationResult(targetTemplate.get, methodsToImplement))
  }

  override type RefactoringParameters = Unit

  override def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters) = {
    import prepared._

    val findTemplate = filter {
      case t: Template => t == targetTemplate
    }

    val addMethods = transform {
      case tpl @ Template(_,  _, body) if tpl == prepared.targetTemplate =>
        val methodsBody = Block(Ident("???") :: Nil, EmptyTree)
        val methodWithRhs = methodsToImplement.map(_ copy (rhs = methodsBody))
        tpl.copy(body = body ++ methodWithRhs).replaces(tpl)
    }

    val transformation = topdown {
      matchingChildren {
        findTemplate &>
        addMethods
      }
    }
    Right(transformFile(selection.file, transformation))
  }
}
