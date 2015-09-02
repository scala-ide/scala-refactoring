package scala.tools.refactoring
package implementations

import scala.tools.refactoring.MultiStageRefactoring

import common.Change

/**
 * Baseclass for refactorings that generate class-level source based on
 * the class parameters.
 */
abstract class ClassParameterDrivenSourceGeneration extends MultiStageRefactoring with common.InteractiveScalaCompiler {

  import global._

  case class PreparationResult(
      classDef: ClassDef,
      classParams: List[(ValDef, Boolean)],
      existingEqualityMethods: List[ValOrDefDef])

  /** A function that takes a class parameter name and decides
   *  whether this parameter should be used in equals/hashCode
   *  computations, a boolean that indicates whether calls
   *  to super should be used or not, and a boolean that indicates
   *  whether existing equality methods (equals, canEqual and hashCode)
   *  should be kept or replaced.
   */
  case class RefactoringParameters(
      callSuper: Boolean = true,
      paramsFilter: ValDef => Boolean,
      keepExistingEqualityMethods: Boolean)

  def prepare(s: Selection) = {
    val notAClass = Left(PreparationError("No class definition selected."))
    s.findSelectedOfType[ClassDef] match {
      case None => notAClass
      case Some(classDef) if classDef.hasSymbol && classDef.symbol.isTrait => notAClass
      case Some(classDef) => {
        failsBecause(classDef).map(PreparationError(_)) toLeft {
          val classParams = classDef.impl.nonPrivateClassParameters
          val equalityMethods = classDef.impl.existingEqualityMethods
          PreparationResult(classDef, classParams, equalityMethods)
        }
      }
    }
  }

  def failsBecause(classDef: ClassDef): Option[String]

  def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val selectedParams = prep.classParams.map(_._1) filter params.paramsFilter

    val classSymbol = prep.classDef.symbol

    val templateFilter = filter {
      case prep.classDef.impl => true
    }

    val refactoring = topdown {
      matchingChildren {
        templateFilter &> sourceGeneration(selectedParams, prep, params)
      }
    }

    Right(transformFile(selection.file, refactoring))
  }

  def sourceGeneration(params: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters): Transformation[Tree, Tree]

}