/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring

case class RefactoringError(cause: String) extends Exception(cause)
