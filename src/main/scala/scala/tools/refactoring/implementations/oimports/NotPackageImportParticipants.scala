package scala.tools.refactoring
package implementations.oimports

import implementations.OrganizeImports
import scala.annotation.tailrec

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
      val selects = mutable.ListBuffer[(Symbol, Select)]()
      val selectsTraverser = new TraverserWithFakedTrees { self =>
        private val default: PartialFunction[Tree, Unit] = {
          case t => super.traverse(t)
        }

        private val special: PartialFunction[Tree, Unit] = {
          case s @ Select(qual, _) =>
            selects += (currentOwner -> s)
            traverse(qual)
          case TypeDef(_, _, compoundTypeDefs, rhs) =>
            rhs :: compoundTypeDefs foreach traverse
        }

        override def traverse(tree: Tree): Unit = special.orElse {
          new ImplicitValDefTraverserPF[organizeImportsInstance.type](organizeImportsInstance)(self)
        }.orElse {
          default
        }(tree)

        override def handleCompoundTypeTree(parents: List[Tree], parentTypes: List[Type]): Unit = parents zip parentTypes foreach {
          case (AppliedTypeTree(tpt, _), tpe @ TypeRef(_, sym, _)) => tpt match {
            case i: Ident if i.tpe == null =>
              fakeSelectTree(tpe, sym, i) foreach traverse
            case tree =>
              super.handleCompoundTypeTree(parents, parentTypes)
          }
          case tree =>
            super.handleCompoundTypeTree(parents, parentTypes)
        }
      }
      selectsTraverser.traverse(treeWithoutImports(block))
      selects.toList
    }

    protected def doApply(trees: List[Import]) = trees.iterator.collect {
      case imp @ Import(importQualifier, importSelections) =>
        val importSym = importQualifier.symbol.fullName
        val impOwner = imp match {
          case ro: RegionOwner => Option(ro.owner)
          case _ => None
        }
        val usedSelectors = importSelections filter { importSel =>
          val isWildcard = importSel.name == nme.WILDCARD
          val importSelNames = Set(importSel.name, importSel.rename).filterNot { _ == null }.map { _.toString }
          allSelects.exists { select =>
            val (owner, foundSel) = select
            def downToPackage(selectQualifierSymbol: Symbol): Symbol =
              if (selectQualifierSymbol == null || selectQualifierSymbol == NoSymbol
                || selectQualifierSymbol.isPackage || selectQualifierSymbol.isModule
                || selectQualifierSymbol.isStable)
                selectQualifierSymbol
              else
                downToPackage(selectQualifierSymbol.owner)
            val foundNames = Set(foundSel.name.toString, foundSel.symbol.owner.nameString)
            val foundSym = Option(downToPackage(foundSel.qualifier.symbol)).getOrElse(downToPackage(foundSel.symbol))
            (isWildcard || (foundNames & importSelNames).nonEmpty) &&
              foundSym != null && foundSym.fullName == importSym && impOwner.map { owner.ownerChain.contains }.getOrElse(true)
          }
        }
        (imp, usedSelectors)
    }.collect {
      case (imp, selectors @ h :: _) => imp.copy(selectors = selectors).setPos(imp.pos)
    }.toList
  }

  object RemoveDuplicatedByWildcard extends Participant {
    private def renamed(selector: ImportSelector): Boolean =
      selector.rename != null && selector.name != selector.rename

    protected def doApply(trees: List[Import]) = trees.map { imp =>
      val wild = imp.selectors.find(_.name == nme.WILDCARD)
      if (wild.nonEmpty) {
        val newImp = imp.copy(selectors = imp.selectors.filter { renamed }.sortBy { _.name } ::: wild.toList).setPos(imp.pos)
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

  class CollapseImports[T <: TreeToolbox[organizeImportsInstance.global.type]](val ttb: T) extends Participant {
    @tailrec private def isSame(acc: Boolean)(left: Symbol, right: Symbol): Boolean = {
      val left_ = Option(left).getOrElse(NoSymbol)
      val right_ = Option(right).getOrElse(NoSymbol)
      if (left_ == NoSymbol && right_ == NoSymbol)
        acc
      else
        isSame(left_.nameString == right_.nameString && acc)(left_.owner, right_.owner)
    }

    protected def doApply(trees: List[Import]) = {
      trees.foldRight(Nil: List[ttb.global.Import]) {
        case (imp: ttb.RegionImport, (x: ttb.RegionImport) :: xs) if isSame(true)(imp.expr.symbol, x.expr.symbol) =>
          x.merge(imp) :: xs
        case (imp: ttb.global.Import, xs) =>
          imp :: xs
      }
    }
  }

  class ExpandImports[T <: TreeToolbox[organizeImportsInstance.global.type]](val ttb: T) extends Participant {
    protected def doApply(trees: List[Import]) = {
      trees.flatMap {
        case imp @ ttb.RegionImport(_, selectors) if !selectors.exists(wildcardImport) =>
          imp.spawn
        case imp =>
          List(imp)
      }
    }
  }
}
