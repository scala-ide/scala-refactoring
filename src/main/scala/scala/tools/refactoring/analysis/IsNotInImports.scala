package scala.tools.refactoring
package analysis

trait IsNotInImports { _: CompilationUnitDependencies with common.PimpedTrees =>
  import global._

  private def collectPotentialOwners(of: Select, inWholeTree: Tree): List[Symbol] = {
    val upToPosition = of.pos.start
    val Of = of
    var owners = List.empty[Symbol]
    val collectPotentialOwners = new Traverser {
      var owns = List.empty[Symbol]
      override def traverse(t: Tree) = {
        owns = currentOwner :: owns
        t match {
          case t if !t.pos.isRange || t.pos.start > upToPosition =>
          case Of => owners = owns.distinct
          case t =>
            super.traverse(t)
            owns = owns.tail
        }
      }
    }
    collectPotentialOwners.traverse(inWholeTree)
    owners
  }

  def isSelectNotInRelativeImports(tested: Select, wholeTree: Tree): Boolean = {
    val doesNameFitInTested = compareNameWith(tested)(_)
    val nonPackageOwners = collectPotentialOwners(tested, wholeTree).filterNot { _.hasPackageFlag }
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
