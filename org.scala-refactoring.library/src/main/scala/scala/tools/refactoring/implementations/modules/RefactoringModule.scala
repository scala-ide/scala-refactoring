package scala.tools.refactoring.implementations.modules

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.transformation.TreeTransformations
import scala.tools.refactoring.common.PimpedTrees

/**
 * 
 */
trait RefactoringModule extends CompilerAccess with PimpedTrees with TreeTransformations{
  def preparationError: Option[String] = None
}