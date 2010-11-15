/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile

trait InteractiveScalaCompiler extends CompilerAccess {
  
  val global: tools.nsc.interactive.Global
  
  def compilationUnitOfFile(f: AbstractFile) = global.unitOfFile.get(f)
}
