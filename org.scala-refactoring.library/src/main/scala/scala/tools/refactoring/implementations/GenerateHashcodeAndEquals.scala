package scala.tools.refactoring
package implementations

/**
 * Refactoring that generates hashCode and equals implementations 
 * following the recommendations given in chapter 28 of
 * Programming in Scala.
 */
abstract class GenerateHashcodeAndEquals extends ClassParameterDrivenSourceGeneration {

  import global._
  
  override def failsBecause(classDef: ClassDef) =  {
    if(classDef.impl.hasEqualsOrHashcode)
      Some("equals or hashCode already existing")
    else
      None
  }
  
  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {
    
    val equalityMethods = mkEqualityMethods(preparationResult.classDef.symbol, selectedParams, refactoringParams.callSuper)
    def addEqualityMethods = transform {
      case t @ Template(_, _, body) => t copy (body = equalityMethods:::body) replaces t
    }
    
    addEqualityMethods
  }
  
  def mkEqualityMethods(classSymbol: Symbol, params: List[ValDef], callSuper: Boolean) = {
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, params, callSuper)
    val equals = mkEquals(classSymbol, params, callSuper)
    
    canEqual::equals::hashcode::Nil
  }
  
}