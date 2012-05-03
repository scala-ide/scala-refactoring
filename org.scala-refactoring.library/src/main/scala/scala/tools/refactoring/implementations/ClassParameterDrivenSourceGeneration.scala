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
  
  case class PreparationResult(classDef: ClassDef, classParams: List[(ValDef, Boolean)])
  
  /** A function that takes a class parameter name and decides
   *  whether this parameter should be used in equals/hashCode
   *  computations, and a boolean that indicates whether calls
   *  to super should be used or not.
   */
  case class RefactoringParameters(callSuper: Boolean = true, paramsFilter: Option[ValDef => Boolean] = None)
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classDef) => {
        failsBecause(classDef).map(PreparationError(_)) toLeft {
          PreparationResult(classDef, classDef.impl.nonPrivateClassParameters)
        }
      }
    }
  }
  
  def failsBecause(classDef: ClassDef): Option[String]
  
  def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val selectedParams = params.paramsFilter match {
      case Some(paramsFilter) => prep.classParams.map(_._1) filter paramsFilter
      // if no params filter is supplied we use only immutable class parameters
      case None => prep.classParams.collect{ case (param, false) => param }
    }
    
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