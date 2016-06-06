package scala.tools.refactoring
package implementations.oimports

import implementations.OrganizeImports
import scala.annotation.tailrec

class NotPackageImportParticipants[O <: OrganizeImports](val organizeImportsInstance: O) {
  import organizeImportsInstance._
  import organizeImportsInstance.global._

  class RemoveUnused(block: Tree, addNewImports: List[(String, String)] = Nil) extends Participant {
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

    private def fullNameButSkipPackageObject(sym: Symbol): String = {
      var b: java.lang.StringBuffer = null
      def loop(size: Int, sym: Symbol): Unit = {
        val symName = sym.name
        val nSize = symName.length - (if (symName.endsWith(nme.LOCAL_SUFFIX_STRING)) 1 else 0)
        if (sym.isRoot || sym.isRootPackage || sym == NoSymbol || sym.owner.isEffectiveRoot) {
          val capacity = size + nSize
          b = new java.lang.StringBuffer(capacity)
          b.append(chrs, symName.start, nSize)
        } else {
          loop(size + nSize + 1, sym.effectiveOwner.enclClass)
          if (!sym.isPackageObject) {
            b.append(".")
            b.append(chrs, symName.start, nSize)
          }
        }
      }
      loop(0, sym)
      b.toString
    }

    private val isScalaLanguageImport = MiscTools.isScalaLanguageImport(organizeImportsInstance.global)

    private def isInNewImports(imp: Import): Boolean = imp match {
      case Import(expr, sels) =>
        val ex = importSymbol(expr)
        val tups = sels.collect {
          case ImportSelector(name, _, _, _) => ex -> name.decoded
        }
        tups.intersect(addNewImports).nonEmpty
    }

    private def importSymbol(expr: Tree): String = expr.symbol match {
      case sym if sym != null && sym != NoSymbol => sym.fullName
      case _ => expr.nameString
    }

    protected def doApply(trees: List[Import]) = trees.iterator.collect {
      case imp @ Import(importQualifier, importSelections) =>
        val importSym = importSymbol(importQualifier)
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
              foundSym != null && fullNameButSkipPackageObject(foundSym) == importSym && impOwner.map { impOwner =>
                owner.ownerChain.exists { selectOwner =>
                  selectOwner.name.decoded == impOwner.name.decoded
                }
              }.getOrElse(true)
          } || isScalaLanguageImport(imp) || isInNewImports(imp)
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

    private def isSameExpr(leftExpr: Tree, rightExpr: Tree): Boolean =
      isSame(true)(leftExpr.symbol, rightExpr.symbol) || leftExpr.toString == rightExpr.toString

    protected def doApply(trees: List[Import]) = {
      val collapsed = trees.foldRight(Nil: List[ttb.global.Import]) {
        case (imp: ttb.RegionImport, (x: ttb.RegionImport) :: xs) if isSameExpr(imp.expr, x.expr) =>
          x.merge(imp) :: xs
        case (imp: ttb.global.Import, xs) =>
          imp :: xs
      }
      collapsed
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

  case class AlwaysUseWildcards[T <: TreeToolbox[organizeImportsInstance.global.type]](val ttb: T)(imports: Set[String]) extends Participant {
    protected def doApply(trees: List[Import]) = {
      val seen = collection.mutable.HashSet[String]()
      trees flatMap {
        case imp @ ttb.RegionImport(qual, selectors) if imports.contains(asSelectorString(qual)) && !selectors.exists { sel =>
          sel.rename != null && sel.name != sel.rename
        } =>
          if (seen.contains(asSelectorString(qual))) {
            None
          } else {
            seen += asSelectorString(qual)
            Some(imp.copy(qual, List(ImportSelector(nme.WILDCARD, -1, nme.WILDCARD, -1))).copyAttrs(imp))
          }
        case t => Some(t)
      }
    }
  }
}
