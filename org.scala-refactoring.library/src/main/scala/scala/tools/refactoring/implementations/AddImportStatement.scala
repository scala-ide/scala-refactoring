/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.{InteractiveScalaCompiler, Change}
import transformation.TreeFactory
import scala.tools.nsc.io.AbstractFile

abstract class AddImportStatement extends Refactoring with TreeFactory with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global

  def addImport(file: AbstractFile, fqName: String): List[Change] = addImports(file, List(fqName))

  def addImports(file: AbstractFile, importsToAdd: Iterable[String]): List[Change] = {

    import global._
    
    val addImportStatement = once(locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {
      case (p, imports, others) =>
        val SplitAtDot = "(.*)\\.(.*?)".r
        val importTrees = importsToAdd.map {
          case SplitAtDot(pkg, name) => mkImportFromStrings(pkg, name)
        }.toList
        p copy (stats = (imports ::: importTrees ::: others)) replaces p
    })
    
    val astRoot = abstractFileToTree(file)
    
    val changes = {
      // first try it at the top level to avoid traversing
      // the complete AST
      addImportStatement |>
      topdown(matchingChildren(addImportStatement))
    } apply astRoot

    refactor(changes.toList)
  }
  
  @deprecated("Use addImport(file, ..) instead", "0.4.0")
  def addImport(selection: Selection, fullyQualifiedName: String): List[Change] = {
    addImport(selection.file, fullyQualifiedName)
  }
  
  @deprecated("Use addImport(file, ..) instead", "0.4.0")
  def addImport(selection: Selection, pkg: String, name: String): List[Change] = {
    addImport(selection.file, pkg +"."+ name)
  }
}
