package scala.tools.refactoring.implementations

import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.refactoring.{MultiStageRefactoring, analysis}

abstract class ImplementMethods extends MultiStageRefactoring with analysis.Indexes with InteractiveScalaCompiler {

  import global._

  override type PreparationResult = Seq[DefDef]

  override def prepare(s: Selection): Either[PreparationError, PreparationResult] = {
    val methodsToImplement = for {
      selectedClassDeclaration <- index.declaration(s.enclosingTree.symbol).toSeq collect {
        case traitDeclaration: ClassDef => traitDeclaration
      }
      unimplementedMethod <- selectedClassDeclaration.impl.body collect {
        case methodDeclaration: DefDef if methodDeclaration.rhs.isEmpty => methodDeclaration
      }
    } yield unimplementedMethod
    if(methodsToImplement.isEmpty) Left {
      PreparationError("There are not methods to implement")
    }
    else Right(methodsToImplement)
  }

  override type RefactoringParameters = Unit

  override def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters) = {
    ???
  }
}
