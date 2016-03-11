package scala.tools.refactoring
package implementations.oimports

import scala.tools.refactoring.implementations.OrganizeImports

class NotPackageImportParticipants[O <: OrganizeImports](val organizeImportsInstance: O) {
  import organizeImportsInstance._
  import organizeImportsInstance.global._

  class RemoveUnused(block: Tree) extends Participant {
    private def treeWithoutImports(tree: Tree) = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Import(_, _) => EmptyTree
        case t => super.transform(t)
      }
    }.transform(tree)

    private lazy val allSelects = {
      import scala.collection.mutable
      val selects = mutable.ListBuffer[Select]()
      val selectsTraverser = new TraverserWithFakedTrees {
        override def traverse(tree: Tree): Unit = tree match {
          case s @ Select(qual, _) =>
            selects += s
            traverse(qual)
          case t => super.traverse(t)
        }
      }
      selectsTraverser.traverse(treeWithoutImports(block))
      selects.toList
    }

    protected def doApply(trees: List[Import]) = trees collect {
      case imp @ Import(importQualifier, importSelections) =>
        val usedSelectors = importSelections filter { importSel =>
          val importSym = importQualifier.symbol.fullName
          val isWildcard = importSel.name == nme.WILDCARD
          val importSelNames = Set(importSel.name, importSel.rename).filterNot { _ == null }.map { _.toString }

          allSelects.exists { foundSel =>
            def downToPackage(selectQualifierSymbol: Symbol): Symbol =
              if (selectQualifierSymbol == null || selectQualifierSymbol == NoSymbol
                  || selectQualifierSymbol.isPackage || selectQualifierSymbol.isModule
                  || selectQualifierSymbol.isStable)
                selectQualifierSymbol
              else
                downToPackage(selectQualifierSymbol.owner)
            val foundNames = Set(foundSel.name.toString, foundSel.symbol.owner.nameString)
            val foundSym = Option(downToPackage(foundSel.qualifier.symbol)).getOrElse(downToPackage(foundSel.symbol))
            (isWildcard || foundNames.&(importSelNames).nonEmpty) &&
              foundSym != null && foundSym.fullName == importSym
          }
        }
        val result = usedSelectors match {
          case Nil => null
          case s => imp.copy(selectors = usedSelectors).setPos(imp.pos)
        }
        result
    } filter { _ ne null }
  }

  object RemoveDuplicatedByWildcard extends Participant {
    protected def doApply(trees: List[Import]) = trees.map { imp =>
      val wild = imp.selectors.find(_.name == nme.WILDCARD)
      if (wild.nonEmpty) {
        val newImp = imp.copy(selectors = wild.toList).setPos(imp.pos)
        newImp.symbol = imp.symbol
        newImp
      } else
        imp
    }.groupBy {
      _.expr.toString
    }.collect {
      case (_, imports) =>
        val (wild, rest) = imports.partition(_.selectors.exists(_.name == nme.WILDCARD))
        if (wild.nonEmpty)
          wild
        else
          rest
    }.flatten.toList
  }
}
