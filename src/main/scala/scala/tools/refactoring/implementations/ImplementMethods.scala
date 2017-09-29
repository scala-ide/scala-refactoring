package scala.tools.refactoring.implementations

import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.{MultiStageRefactoring, analysis}

abstract class ImplementMethods extends MultiStageRefactoring with analysis.Indexes with TreeFactory with InteractiveScalaCompiler {

  import global._

  case class PreparationResult(targetTemplate: Template, methodsToImplement: Seq[DefDef])

  /* Helper class to box methods so they can be
     compared in terms of their signature.
   */
  implicit class OverloadedMethod(val method: DefDef) {

    private val key = {
      import method.{name, tparams, vparamss}

      val vparamsTypes = for {
        paramList <- vparamss
        param <- paramList
      } yield param.tpt.tpe

      (name, tparams, vparamsTypes)

    }

    override def hashCode(): RunId = key.hashCode()

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: OverloadedMethod => key == that.key
      case _ => false
    }
  }

  private def templateAncestry(template: Template): List[Template] =
    template :: {
      for {
        parent <- template.parents
        parentImp <- index.declaration(parent.symbol).toList collect {
          case ClassDef(_, _, _, impl) => impl
        }
        ancestor <- templateAncestry(parentImp)
      } yield ancestor
    }

  override def prepare(s: Selection): Either[PreparationError, PreparationResult] = {


    // Expand the selection to the concrete type when a kind was initially selected.
    val maybeSelectedTemplate = (s::s.expandToNextEnclosingTree.toList) flatMap { sel: Selection =>
      index.declaration(sel.enclosingTree.symbol)
    } collectFirst {
      case templateDeclaration: ClassDef => templateDeclaration.impl
    }

    // Get a sequence of methods found in the selected mixed trait.
    val methodsToImplement = {

      val rawList = for {
        selectedTemplate <- maybeSelectedTemplate.toList
        selectedDeclaration <- templateAncestry(selectedTemplate)
        unimplementedMethod <- selectedDeclaration.body collect {
          case methodDeclaration: DefDef if methodDeclaration.rhs.isEmpty =>
            methodDeclaration
        }
      } yield unimplementedMethod;

      val (uniqueMethods, _) =
        rawList.foldRight((List.empty[DefDef], Set.empty[OverloadedMethod])) {
          case (method, (l, visited)) if !visited.contains(method) =>
            (method::l, visited + method)
          case (_, st) => st
        }
      uniqueMethods
    }

    // Use the selection to find the template where the methods should be implemented.
    val targetTemplate = s.expandToNextEnclosingTree.flatMap {
      _.selectedSymbolTree collect {
        case target: Template => target
      }
    }

    targetTemplate map { t => // If the selection has indeed a target template...
      if(methodsToImplement.isEmpty) Left { //... as long as there are methods in the mixed trait...
        PreparationError("There are not methods to implement")
      } else Right { //... these are selected to be defined in the target template.
        // If and only if they're not already defined there.
        val implementedMethods: Set[OverloadedMethod] = {
          t.body collect {
            case methodDef: DefDef => new OverloadedMethod(methodDef)
          } toSet
        }
        PreparationResult(t, methodsToImplement.filterNot(implementedMethods contains _))
      }
    } getOrElse Left {
      PreparationError("No target class in selection")
    }
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
