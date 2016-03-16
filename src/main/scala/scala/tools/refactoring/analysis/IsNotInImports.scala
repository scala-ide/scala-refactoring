package scala.tools.refactoring
package analysis

class IsNotInImports[C <: CompilationUnitDependencies with common.EnrichedTrees](val cuDependenciesInstance: C) {
  import cuDependenciesInstance.global._

  def apply(tree: Tree) = new IsSelectNotInRelativeImports(tree)

  class IsSelectNotInRelativeImports(wholeTree: Tree) {
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
      val Select(testedQual, testedName) = tested
      val testedQName = List(testedQual.nameString, testedName).mkString(".")
      val Import(thatQual, thatSels) = that
      val impNames = thatSels.map { sel =>
        if (sel.name == nme.WILDCARD) thatQual.nameString
        else List(thatQual.nameString, sel.name).mkString(".")
      }
      impNames.exists { testedQName.startsWith }
    }
  }
}
