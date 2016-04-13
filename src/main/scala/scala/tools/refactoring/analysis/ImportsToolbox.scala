package scala.tools.refactoring
package analysis

/** Class to wrap path dependent type on `CompilationUnitDependencies` used in `CompilationUnitDependencies`. */
class ImportsToolbox[C <: CompilationUnitDependencies with common.EnrichedTrees](val cuDependenciesInstance: C) {
  import cuDependenciesInstance.global._

  def apply(tree: Tree) = new IsSelectNotInRelativeImports(tree)

  /** Checks if for given `Select` potentially done from `TypeTree` exists import (represented by `Import`)
   *  in this `Select` scope or its parent.
   */
  class IsSelectNotInRelativeImports(wholeTree: Tree) {
    private def collectPotentialOwners(of: Select): List[Symbol] = {
      var owners = List.empty[Symbol]
      def isSelectEmbracedByTree(tree: Tree): Boolean =
        tree.pos.isRange && tree.pos.start <= of.pos.start && of.pos.end <= tree.pos.end
      val collectPotentialOwners = new Traverser {
        var owns = List.empty[Symbol]
        override def traverse(t: Tree) = {
          owns = currentOwner :: owns
          t match {
            case potential if isSelectEmbracedByTree(potential) =>
              owners = owns.distinct
              super.traverse(t)
            case t =>
              super.traverse(t)
              owns = owns.tail
          }
        }
      }
      collectPotentialOwners.traverse(wholeTree)
      owners
    }

    /** Returns `true` if import has been found for tested `Select` and `false` otherwise.
     *  Examples:
     *  {{{
     *  trait A {
     *    import a.b
     *    def foo = {
     *      val baz: b = ???
     *    }
     *  }
     *  }}}
     *  returns `true`
     *  {{{
     *  def foo = {
     *    import a.b
     *    val baz: b = ???
     *  }
     *  }}}
     *  returns `true`
     *  but
     *  {{{
     *  def foo = {
     *    val baz: b = ???
     *    import a.b
     *  }
     *  }}}
     *  returns `false`
     *  For more see tests suites.
     */
    def apply(tested: Select): Boolean = {
      val doesNameFitInTested = compareNameWith(tested) _
      val nonPackageOwners = collectPotentialOwners(tested).filterNot { _.hasPackageFlag }
      def isValidPosition(t: Import): Boolean = t.pos.isRange && t.pos.start < tested.pos.start
      val isImportForTested = new Traverser {
        var found = false
        override def traverse(t: Tree) = t match {
          case imp: Import if isValidPosition(imp) && doesNameFitInTested(imp) && nonPackageOwners.contains(currentOwner) =>
            found = true
          case t => super.traverse(t)
        }
      }
      isImportForTested.traverse(wholeTree)
      !isImportForTested.found
    }

    private def compareNameWith(tested: Select)(that: Import): Boolean = {
      import cuDependenciesInstance.additionalTreeMethodsForPositions
      def mkName(t: Tree) = if (t.symbol != null && t.symbol != NoSymbol) t.symbol.fullNameString else t.nameString
      val Select(testedQual, testedName) = tested
      val testedQName = List(mkName(testedQual), testedName).mkString(".")
      val Import(thatQual, thatSels) = that
      val impNames = thatSels.flatMap { sel =>
        if (sel.name == nme.WILDCARD) List(mkName(thatQual))
        else Set(sel.name, sel.rename).map { name => List(mkName(thatQual), name).mkString(".") }.toList
      }
      impNames.exists { testedQName.startsWith }
    }
  }
}
