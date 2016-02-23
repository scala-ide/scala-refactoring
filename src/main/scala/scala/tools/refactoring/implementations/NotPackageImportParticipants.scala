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
      val selects = mutable.ListBuffer[Select]()
      val selectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case s @ Select(qual, _) =>
            selects += s
            traverse(qual)
          case t => super.traverse(t)
        }
      }
      selectsTraverser.traverse(treeWithoutImports(block.asInstanceOf[Tree]))
      selects.toList
    }

    protected def doApply(trees: List[organizeImportsInstance.global.Import]) = trees.asInstanceOf[List[Import]] collect {
      case imp @ Import(importQualifier: Select, importSelections) =>
        val usedSelectors = importSelections filter { importSel =>
          val importName = importSel.name.toString
          val importSym = importQualifier.symbol
          val isWildcard = importSel.name == nme.WILDCARD

          allSelects.exists { foundSel =>
            val foundName = foundSel.symbol.nameString
            val foundSym = foundSel.qualifier.symbol
            (isWildcard || foundName == importName) && foundSym == importSym
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
