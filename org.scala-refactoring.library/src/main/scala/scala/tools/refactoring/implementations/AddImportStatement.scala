/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.InteractiveScalaCompiler
import common.Change
import transformation.TreeFactory
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.common.TextChange

abstract class AddImportStatement extends Refactoring with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global

  def addImport(file: AbstractFile, fqName: String): List[TextChange] = addImports(file, List(fqName))

  def addImports(file: AbstractFile, importsToAdd: Iterable[String]): List[TextChange] = {

    val astRoot = abstractFileToTree(file)

    refactor((addImportTransformation(importsToAdd) apply astRoot).toList) collect {
      case tc: TextChange => tc
    }
  }

  @deprecated("Use addImport(file, ..) instead", "0.4.0")
  def addImport(selection: Selection, fullyQualifiedName: String): List[Change] = {
    addImport(selection.file, fullyQualifiedName)
  }

  @deprecated("Use addImport(file, ..) instead", "0.4.0")
  def addImport(selection: Selection, pkg: String, name: String): List[Change] = {
    addImport(selection.file, pkg +"."+ name)
  }

  @deprecated("Not needed anymore, don't override.", "0.4.0")
  def getContentForFile(file: AbstractFile): Array[Char] = throw new UnsupportedOperationException
}
