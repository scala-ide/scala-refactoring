package scala.tools.refactoring
package analysis

import tools.nsc.interactive.Global

class IsNotInImports(val global: Global, val cuDependenciesInstance: CompilationUnitDependencies with common.EnrichedTrees, tree: Global#Tree) {
  import global._
  val wholeTree = tree.asInstanceOf[Tree]

  private def collectPotentialOwners(of: Select): List[Symbol] = {
    val upToPosition = of.pos.start
    def isThisSelectTree(fromWholeTree: Tree): Boolean =
      fromWholeTree.pos.isRange && fromWholeTree.pos.start == upToPosition
    var owners = List.empty[Symbol]
    val collectPotentialOwners = new Traverser {
      var owns = List.empty[Symbol]
      override def traverse(t: Tree) = {
        owns = currentOwner :: owns
        t match {
          case t if !t.pos.isRange || t.pos.start > upToPosition =>
          case potential if isThisSelectTree(potential) =>
            owners = owns.distinct
          case t =>
            super.traverse(t)
            owns = owns.tail
        }
      }
    }
    collectPotentialOwners.traverse(wholeTree)
    owners
  }

  def isSelectNotInRelativeImports(tested: Global#Select): Boolean = {
    val doesNameFitInTested = compareNameWith(tested) _
    val nonPackageOwners = collectPotentialOwners(tested.asInstanceOf[Select]).filterNot { _.hasPackageFlag }
    def isValidPosition(t: Import): Boolean = t.pos.isRange && t.pos.start < tested.pos.start
    val isImportForTested = new Traverser {
      var found = false
      override def traverse(t: Tree) = t match {
        case imp: Import if isValidPosition(imp) && doesNameFitInTested(imp) && nonPackageOwners.contains(currentOwner) =>
          found = true
        case t => super.traverse(t)
      }
    }
    isImportForTested.traverse(wholeTree.asInstanceOf[Tree])
    !isImportForTested.found
  }

  private def compareNameWith(tested: Global#Select)(that: Global#Import): Boolean = {
    import cuDependenciesInstance.additionalTreeMethodsForPositions
    val Select(testedQual, testedName) = tested
    val testedQName = List(testedQual.asInstanceOf[cuDependenciesInstance.global.Tree].nameString, testedName).mkString(".")
    val Import(thatQual, thatSels) = that
    val impNames = thatSels.map { sel =>
      if (sel.name == nme.WILDCARD) thatQual.asInstanceOf[cuDependenciesInstance.global.Tree].nameString
      else List(thatQual.asInstanceOf[cuDependenciesInstance.global.Tree].nameString, sel.name).mkString(".")
    }
    impNames.exists { testedQName.startsWith }
  }
}
