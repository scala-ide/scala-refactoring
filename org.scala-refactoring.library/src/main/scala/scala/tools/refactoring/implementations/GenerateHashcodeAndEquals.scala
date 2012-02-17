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
abstract class GenerateHashcodeAndEquals extends ClassParameterDrivenSourceGeneration {

  import global._
  
  override def failsBecause(classDef: ClassDef) = super.failsBecause(classDef) match {
    case None => {
      if(classDef.impl.hasEqualsOrHashcode)
        Some("equals or hashCode already existing")
      else
        None
    }
    case fails => fails
  }
  
  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {
    val superGeneration = super.sourceGeneration(selectedParams, preparationResult, refactoringParams)
    
    val equalityMethods = mkEqualityMethods(preparationResult.classDef.symbol, selectedParams, refactoringParams.callSuper)
    def addEqualityMethods = transform {
      case t @ Template(parents, self, body) => Template(parents, self, equalityMethods:::body) replaces t
    }
    
    superGeneration &> addEqualityMethods
  }
  
  def mkEqualityMethods(classSymbol: Symbol, params: List[ValDef], callSuper: Boolean) = {
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, params, callSuper)
    val equals = mkEquals(classSymbol, params, callSuper)
    
    canEqual::equals::hashcode::Nil
  }
  
}