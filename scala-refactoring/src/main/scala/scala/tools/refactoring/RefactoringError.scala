package scala.tools.refactoring

case class RefactoringError(cause: String) extends Exception(cause)
