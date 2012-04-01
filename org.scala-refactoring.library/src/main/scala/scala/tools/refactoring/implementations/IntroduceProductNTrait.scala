package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.transformation.TreeFactory

/**
 * Refactoring that implements the ProductN trait for a class.
 * Given N selected class parameters this refactoring generates
 * the methods needed to implement the ProductN trait. This includes
 * implementations for hashCode and equals.
 * @see GenerateHashcodeAndEquals
 */
abstract class IntroduceProductNTrait extends GenerateHashcodeAndEquals {

  import global._
  
  
  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {
    val superGeneration = super.sourceGeneration(selectedParams, preparationResult, refactoringParams)

    def mkProjections() = {
      def makeElemProjection(elem: ValDef, pos: Int) = {
        val body = List(Ident(elem.name))
        mkDefDef(name = "_" + pos, body = body)
      }

      selectedParams.zipWithIndex.map(t => makeElemProjection(t._1, t._2 + 1))
    }

    val projections = mkProjections
    
    val arity = selectedParams.length
    val paramsTypenames = selectedParams.map(v => v.tpt.nameString)
    val productParentName = newTermName("Product" + arity + "[" + paramsTypenames.mkString(", ") + "]")
    
    def addProductTrait = transform {
      case t @ Template(parents, self, body) => Template(parents:::List(Ident(productParentName)), self, projections:::body) replaces t
    }
    
    superGeneration &> addProductTrait
  }
  
  

}