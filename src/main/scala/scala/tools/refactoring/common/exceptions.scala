/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

class TreeNotFound(file: String) extends Exception("Tree not found for file "+ file +".")

class RefactoringError(cause: String) extends Exception(cause)