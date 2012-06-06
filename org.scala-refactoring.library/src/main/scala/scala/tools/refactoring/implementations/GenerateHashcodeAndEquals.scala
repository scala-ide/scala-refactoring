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
    if(classDef.impl.hasEqualityMethod)
      Some("An equality method (equals, canEquals or hashCode) already exists.")
    else
      None
  }
  
  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {
    
    val equalityMethods = mkEqualityMethods(preparationResult.classDef.symbol, selectedParams, refactoringParams.callSuper)
    val newParents = newParentNames(selectedParams).map(name => Ident(newTermName(name)))
    def addEqualityMethods = transform {
      case t @ Template(parents, self, body) => Template(parents:::newParents, self, equalityMethods:::body) replaces t
    }
    
    addEqualityMethods
  }
  
  private def mkEqualityMethods(classSymbol: Symbol, params: List[ValDef], callSuper: Boolean) = {
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, params, callSuper)
    val equals = mkEquals(classSymbol, params, callSuper)
    
    canEqual::equals::hashcode::Nil
  }
  
  protected def newParentNames(selectedParams: List[ValDef]): List[String] = {
    "Equals"::Nil
  }
  
}