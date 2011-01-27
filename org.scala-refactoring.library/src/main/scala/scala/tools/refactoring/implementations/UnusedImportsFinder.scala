/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.InteractiveScalaCompiler
import common.{CompilerAccess, ConsoleTracing, SilentTracing, TreeTraverser, PimpedTrees}
import sourcegen.SourceGenerator

trait UnusedImportsFinder extends SourceGenerator with InteractiveScalaCompiler with TreeTraverser with PimpedTrees with SilentTracing {
  
  import global._

  def computeDependentPackageObjectNames(unit: RichCompilationUnit) = unit.depends filter (_.isPackageObjectClass) map (_.tpe.safeToString)
  def computeDependentModules(unit: RichCompilationUnit) = {

    def isType(t: Tree) =
      t.tpe != null && t.tpe != NoType && t.tpe.typeSymbol != NoSymbol

    // a Traverser that ignores imports 
    val importsIgnoringTraverser = new FilterTreeTraverser(isType) {
      override def traverse(tree: Tree): Unit = tree match {
        case Import(_, _) =>
        case _ => super.traverse(tree);
      }
    }

    // we also need all the dependencies of the compilation unit
    val unitDependencies = unit.depends filterNot (s => s.isModuleClass || s == NoSymbol) toList

    (unitDependencies ::: (filterTree(unit.body, importsIgnoringTraverser) map (_.tpe.typeSymbol))) distinct
  }

  def wildcardImport(i: ImportSelector) = i.name == nme.WILDCARD

  def isWildcardImportNeeded(unit: RichCompilationUnit, expr: Tree, s: ImportSelector) = {

    def isDependentModule(m: Symbol) = {
      val moduleName = if (m.isModuleClass) m.fullName else m.owner.fullName
      val importName = expr.symbol.fullName
      moduleName == importName
    }

    expr.symbol == NoSymbol || computeDependentModules(unit).exists(isDependentModule)
  }

  def importSelectorImportsFromNeededPackageObject(unit: RichCompilationUnit, t: Tree) = {
    computeDependentPackageObjectNames(unit).exists { name =>
      val treeString = createText(t)
      name == "object " + treeString + "package"
    }
  }

  def neededImportSelector(unit: RichCompilationUnit, expr: Tree, s: ImportSelector) = {
    (wildcardImport(s) && isWildcardImportNeeded(unit, expr, s)) ||
      computeDependentModules(unit).exists(m => m.name.toString == s.name.toString) ||
      importSelectorImportsFromNeededPackageObject(unit, expr)
  }
}
