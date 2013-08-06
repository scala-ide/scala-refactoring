/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

trait CompilerAccess {

  val global: tools.nsc.Global

  def compilationUnitOfFile(f: AbstractFile): Option[global.CompilationUnit]
}
