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
    None
  }

  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {

    val equalityMethods = mkEqualityMethods(preparationResult.classDef.symbol, selectedParams, refactoringParams.callSuper)
    val newParents = newParentNames(preparationResult.classDef, selectedParams).map(name => Ident(newTermName(name)))

    val equalityMethodNames = List(nme.equals_, nme.hashCode_, nme.canEqual_).map(_.toString)
    def isEqualityMethod(t: Tree) = t match {
      case d: ValOrDefDef => equalityMethodNames contains d.nameString
      case _ => false
    }

    def addEqualityMethods = transform {
      case t @ Template(parents, self, body) => {
        val existing = preparationResult.existingEqualityMethods
        val bodyFilter: Tree => Boolean = refactoringParams.keepExistingEqualityMethods match {
          case true => (t: Tree) => true
          case false => (t: Tree) => !isEqualityMethod(t)
        }
        val filteredBody = body.filter(bodyFilter)
        val equalityMethodsInBody = filteredBody collect {case d: ValOrDefDef if equalityMethodNames contains d.nameString => d.name }
        val filteredEqualityMethods = equalityMethods.filter(e => !(equalityMethodsInBody contains e.name))
        Template(parents:::newParents, self, filteredBody:::filteredEqualityMethods) replaces t
      }
    }

    addEqualityMethods
  }

  private def mkEqualityMethods(classSymbol: Symbol, params: List[ValDef], callSuper: Boolean) = {
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, params, callSuper)
    val equals = mkEquals(classSymbol, params, callSuper)

    canEqual::equals::hashcode::Nil
  }

  protected def newParentNames(classDef: ClassDef, selectedParams: List[ValDef]): List[String] = {
    val existingParents = classDef.impl.parents.map(_.nameString)
    if(existingParents contains "Equals")
      Nil
    else
      "Equals"::Nil
  }

}