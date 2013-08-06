/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.CompilerAccess
import common.PimpedTrees
import common.SilentTracing
import common.TreeTraverser
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
    val unitDependencies = {
      unit.depends.filterNot { s =>

        /*
         * In interactive mode, the compiler contains more information (constants are not inlined) and
         * we can analyze more correctly.  In non-interactive use, we currently don't have a way to do
         * this, so we prefer to get false negatives instead of false positives.
         *
         * This way, when this trait is used during organize imports, an import that was previously
         * not detected by the compiler's code analysis can now be removed.
         * */

        if(global.forInteractive)
          s == NoSymbol || s.isModuleClass
        else
          s == NoSymbol
      }.toList
    }

    val astDependencies = filterTree(unit.body, importsIgnoringTraverser) map (_.tpe.typeSymbol)

    (unitDependencies ::: (astDependencies)).distinct
  }

  def wildcardImport(i: ImportSelector) = i.name == nme.WILDCARD

  def isWildcardImportNeeded(unit: CompilationUnit, dependentModules: List[Symbol], expr: Tree, s: ImportSelector): Boolean = {

    /*
     * Because we cannot yet detect unused wildcard imports from a value,
     * we skip them here. Therefore, imports of the form
     *
     *   import global._
     *
     * are never reported as being unused.
     */
    if(expr.symbol.isValue || expr.symbol.isMethod) return true

    def isDependentModule(m: Symbol) = {
      val moduleName = if (m.isModuleClass) m.fullName else m.owner.fullName
      val importName = expr.symbol.fullName
      moduleName == importName
    }

    expr.symbol == NoSymbol || dependentModules.exists(isDependentModule)
  }

  def importSelectorImportsFromNeededPackageObject(unit: CompilationUnit, t: Tree) = {
    computeDependentPackageObjectNames(unit).exists { name =>
      val treeString = createText(t, None)
      name == "object " + treeString + "package"
    }
  }

  def neededImportSelector(unit: CompilationUnit, expr: Tree, s: ImportSelector): Boolean = {

    /* Never report an import that is renamed to _ as unused. */
    if (s.rename == nme.WILDCARD) return true

    /*
     * We cannot (yet) decide whether an import is needed for all the different kinds of imports.
     *
     * For example, a constant that is imported will get inlined in the build compiler, so we need
     * to skip these. Also, methods that are imported cannot be detected at the moment.
     *
     * */
    val cannotDecideIfNeeded = expr.symbol.info.members filter (_.name == s.name) map (_.info) exists {
      case _: TypeRef =>
        false
      case _ =>
        true
    }

    if(cannotDecideIfNeeded) {
      true // the import is needed
    } else {
      val dependentModules = computeDependentModules(unit)

      (wildcardImport(s) && isWildcardImportNeeded(unit, dependentModules, expr, s)) ||
      dependentModules.exists(m => m.name.toString == s.name.toString) ||
      importSelectorImportsFromNeededPackageObject(unit, expr)
    }
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
