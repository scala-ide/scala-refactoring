/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

/**
 * Many parts of the library can work with the non-interactive global,
 * but some -- most notably the refactoring implementations -- need an
 * interactive compiler, which is expressed by this trait.
 */
trait InteractiveScalaCompiler extends CompilerAccess {
  
  val global: tools.nsc.interactive.Global
  
  def compilationUnitOfFile(f: AbstractFile) = global.unitOfFile.get(f)
}
