/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import common.InteractiveScalaCompiler
import transformation.TreeFactory

abstract class AddImportStatement extends Refactoring with TreeFactory with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global

  def addImport(selection: Selection, fullyQualifiedName: String): List[Change] = {
    val SplitAtDot = "(.*)\\.(.*?)".r
    val SplitAtDot(pkg, name) = fullyQualifiedName
    addImport(selection, pkg, name)
  }

  def addImport(selection: Selection, pkg: String, name: String): List[Change] = {

    import global._
    
    val addImportStatement = locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (p, imports, others) =>
        p copy (stats = (imports ::: List(mkImportFromStrings(pkg, name))) ::: others) replaces p
    }
    
    val astRoot = abstractFileToTree(selection.file)

    val changes = (addImportStatement |> topdown(matchingChildren(addImportStatement))) apply astRoot

    refactor(changes.toList)
  }
}
