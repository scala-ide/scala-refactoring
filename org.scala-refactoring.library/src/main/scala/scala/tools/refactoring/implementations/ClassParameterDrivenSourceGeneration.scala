package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.transformation.TreeFactory
import scala.reflect.generic.ModifierFlags
import scala.tools.refactoring.common.PimpedTrees
import scala.reflect.generic.Flags

/**
 * Baseclass for refactorings that generate class-level source based on
 * the class parameters.
 */
abstract class ClassParameterDrivenSourceGeneration extends MultiStageRefactoring with common.InteractiveScalaCompiler with TreeFactory with PimpedTrees {

  import global._
  
  case class PreparationResult(classDef: ClassDef, classParams: List[(String, Boolean)])
  
  /** A function that takes a class parameter name and decides
   *  whether this parameter should be used in equals/hashCode
   *  computations, and a boolean that indicates whether calls
   *  to super should be used or not.
   */
  case class RefactoringParameters(callSuper: Boolean = true, paramsFilter: Option[String => Boolean] = None)
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classDef) => {
        failsBecause(classDef).map(PreparationError(_)) toLeft {
          PreparationResult(classDef, classDef.impl.nonPrivateClassParameters.map(t => (t._1.nameString, t._2)))
        }
      }
    }
  }
  
  def failsBecause(classDef: ClassDef): Option[String] = None
  
  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val nonPrivateParams = prep.classDef.impl.nonPrivateClassParameters
        
    // if no params filter is supplied in the refactoring parameters we use only immutable class parameters
    lazy val paramsFilter = {
      // collect all immutable class parameters 
      val immutableParams = nonPrivateParams collect { case t if !t._2 => t._1.nameString }
      params.paramsFilter getOrElse ((str: String) => immutableParams contains str)
    }
    
    val selectedParams = selectParams(nonPrivateParams, params.paramsFilter)
    
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
  
  def selectParams(candidates: List[(ValDef, Boolean)], paramsFilter: Option[String => Boolean]) = {
    // if no params filter is supplied we use only immutable class parameters
    val filter = {
      // collect all immutable class parameters 
      lazy val immutableParams = candidates collect { case t if !t._2 => t._1.nameString }
      paramsFilter getOrElse ((str: String) => immutableParams contains str)
    }
    candidates collect { case t if filter(t._1.nameString) => t._1}
  }
  
  def sourceGeneration(params: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters): Transformation[Tree, Tree] = id
    
}