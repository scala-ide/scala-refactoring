/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.InteractiveScalaCompiler
import common.{CompilerAccess, ConsoleTracing, SilentTracing, TreeTraverser, PimpedTrees}
import sourcegen.SourceGenerator

trait UnusedImportsFinder extends SourceGenerator with CompilerAccess with TreeTraverser with PimpedTrees with SilentTracing {
  
  import global._

  def computeDependentPackageObjectNames(unit: CompilationUnit) = unit.depends filter (_.isPackageObjectClass) map (_.tpe.safeToString)
  def computeDependentModules(unit: CompilationUnit) = {

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

  def isWildcardImportNeeded(unit: CompilationUnit, expr: Tree, s: ImportSelector): Boolean = {

    /*
     * Because we cannot yet detect unused wildcard imports from a value, 
     * we skip them here. Therefore, imports of the form
     * 
     *   import global._
     *   
     * are never reported as being unused. 
     */
    if(expr.symbol.isMethod) return true 
    
    def isDependentModule(m: Symbol) = {
      val moduleName = if (m.isModuleClass) m.fullName else m.owner.fullName
      val importName = expr.symbol.fullName
      moduleName == importName
    }

    expr.symbol == NoSymbol || computeDependentModules(unit).exists(isDependentModule)
  }

  def importSelectorImportsFromNeededPackageObject(unit: CompilationUnit, t: Tree) = {
    computeDependentPackageObjectNames(unit).exists { name =>
      val treeString = createText(t)
      name == "object " + treeString + "package"
    }
  }

  def neededImportSelector(unit: CompilationUnit, expr: Tree, s: ImportSelector) = {
    (wildcardImport(s) && isWildcardImportNeeded(unit, expr, s)) ||
      computeDependentModules(unit).exists(m => m.name.toString == s.name.toString) ||
      importSelectorImportsFromNeededPackageObject(unit, expr)
  }
  
  def findUnusedImports(unit: CompilationUnit): List[(String, Int)] = {
    
    val unuseds = new collection.mutable.ListBuffer[(String, Int)]
    
    val traverser = new Traverser {
      override def traverse(tree: Tree) = tree match {
        case Import(expr, selectors) =>
          
          selectors foreach { selector =>
            if(!neededImportSelector(unit, expr, selector)) {
              unuseds += Pair(selector.name.toString, tree.pos.line)
            }
          }
          
        case _ => 
          super.traverse(tree)
      }
    }
    
    traverser.traverse(unit.body)
    
    unuseds.toList
  }
}
