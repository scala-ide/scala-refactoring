package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.transformation.TreeFactory
import scala.reflect.generic.ModifierFlags
import scala.tools.refactoring.common.PimpedTrees
import scala.reflect.generic.Flags

/**
 * Refactoring that generates hashCode and equals implementations 
 * following the recommendations given in chapter 28 of
 * Programming in Scala.
 */
abstract class GenerateHashcodeAndEquals extends MultiStageRefactoring with common.InteractiveScalaCompiler with TreeFactory with PimpedTrees {

  import global._
  
  case class PreparationResult(classDef: ClassDef, classParams: List[(String, Boolean)])
  
  /** A function that takes a class parameter name and decides
   *  whether this parameter should be used in equals/hashCode
   *  computations, and a boolean that indicates whether calls
   *  to super should be used or not. The integer option specifies
   *  the integer to be used in the hashcode computation (defaults
   *  to 41).
   */
  case class RefactoringParameters(callSuper: Boolean = true, paramsFilter: Option[String => Boolean] = None)
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classdef) => {
        if(classdef.impl.hasEqualsOrHashcode) {
          Left(PreparationError("equals or hashCode already existing"))
        } else {
          Right(PreparationResult(classdef, classdef.impl.nonPrivateClassParameters.map(t => (t._1.nameString, t._2))))
        }
      }
    }
  }
  
  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val nonPrivateParams = prep.classDef.impl.nonPrivateClassParameters
        
    // if no params filter is supplied in the refactoring parameters we use only immutable class parameters
    lazy val paramsFilter = {
      // collect all immutable class parameters 
      val immutableParams = nonPrivateParams collect { case t if !t._2 => t._1.nameString }
      params.paramsFilter getOrElse ((str: String) => immutableParams contains str)
    }
    
    val paramsForEqual = nonPrivateParams collect { case t if paramsFilter(t._1.nameString)=> t._1 }
    
    val classSymbol = prep.classDef.symbol
    
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, paramsForEqual, params.callSuper)
    val equals = mkEquals(classSymbol, paramsForEqual, params.callSuper)
    
    val newBody = canEqual::equals::hashcode::prep.classDef.impl.body
    val newTemplate = Template(prep.classDef.impl.parents, prep.classDef.impl.self, newBody) 
    val refactoredTemplate = newTemplate replaces prep.classDef.impl
    
    Right(refactor(List(refactoredTemplate)))
  }
  
  
  
}