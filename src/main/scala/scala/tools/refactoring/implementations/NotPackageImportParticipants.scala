package scala.tools.refactoring
package implementations

import scala.tools.nsc.interactive.Global

class NotPackageImportParticipants(val global: Global, val organizeImportsInstance: OrganizeImports) {
  import global._

  class RemoveUnused(block: Global#Tree) extends organizeImportsInstance.Participant {
    private def treeWithoutImports(tree: Tree) = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Import(_, _) => EmptyTree
        case t => super.transform(t)
      }
    }.transform(tree)

    private lazy val allSelects = {
      import scala.collection.mutable
      val selects = mutable.ListBuffer[Global#Select]()
      val selectsTraverser = new organizeImportsInstance.TraverserWithFakedTrees {
        override def traverse(tree: organizeImportsInstance.global.Tree): Unit = tree match {
          case s @ organizeImportsInstance.global.Select(qual, _) =>
            selects += s
            traverse(qual)
          case t => super.traverse(t)
        }
      }
      selectsTraverser.traverse(treeWithoutImports(block.asInstanceOf[Tree]).asInstanceOf[organizeImportsInstance.global.Tree])
      selects.toList
    }

    protected def doApply(trees: List[organizeImportsInstance.global.Import]) = trees.asInstanceOf[List[Import]] collect {
      case imp @ Import(importQualifier, importSelections) =>
        val usedSelectors = importSelections filter { importSel =>
          val importSym = importQualifier.symbol.fullName
          val isWildcard = importSel.name == nme.WILDCARD
          val importSelNames = Set(importSel.name, importSel.rename).filterNot { _ == null }.map { _.toString }

          allSelects.exists { foundSel =>
            def downToPackage(selectQualifierSymbol: Global#Symbol): Global#Symbol =
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
          case Nil => Import(EmptyTree, Nil)
          case _ => imp.copy(selectors = usedSelectors)
        }
        result.asInstanceOf[organizeImportsInstance.global.Import]
    }
  }

  object RemoveDuplicatedByWildcard extends organizeImportsInstance.Participant {
    protected def doApply(trees: List[organizeImportsInstance.global.Import]) = trees.asInstanceOf[List[Import]].map { imp =>
      val wild = imp.selectors.find(_.name == nme.WILDCARD)
      if (wild.nonEmpty)
        imp.copy(selectors = wild.toList)
      else
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
    }.flatten.toList.asInstanceOf[List[organizeImportsInstance.global.Import]]
  }
}
