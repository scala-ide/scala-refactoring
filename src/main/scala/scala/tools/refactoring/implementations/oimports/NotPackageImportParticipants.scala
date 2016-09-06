package scala.tools.refactoring
package implementations.oimports

import implementations.OrganizeImports

class AllSelects[O <: OrganizeImports](val oi: O) {
  import oi._
  import oi.global._
  import scala.collection.mutable

  private def treeWithoutImports(tree: Tree) = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Import(_, _) => EmptyTree
      case t => super.transform(t)
    }
  }.transform(tree)

  def apply(block: Tree): List[(Symbol, Select)] = {
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
        case t: TypeTree if t.original == null && t.tpe.isInstanceOf[TypeRef] && t.attachments.all.nonEmpty =>
          def mkSelects(ttpe: TypeRef): List[Tree] = {
            val currentSelect = self.fakeSelectTreeFromType(ttpe, ttpe.sym, t.attachments.pos)
            val typeRefArgs = ttpe.args.collect {
              case arg: TypeRef => arg
            }
            if (typeRefArgs.isEmpty)
              currentSelect :: Nil
            else {
              currentSelect :: typeRefArgs.flatMap { mkSelects }
            }
          }
          val selects = mkSelects(t.tpe.asInstanceOf[TypeRef])
          selects.foreach { traverse }
        case treeWithAnnotation if annotations(treeWithAnnotation).nonEmpty =>
          annotations(treeWithAnnotation).foreach { ann =>
            ann.args.foreach { traverse }
          }
          super.traverse(treeWithAnnotation)
        case t @ Literal(Constant(value: TypeRef)) =>
          val tree = self.fakeSelectTreeFromType(value, value.sym, t.pos) match {
            case classOfIdent: Ident =>
              val realText = classOfIdent.pos.source.content.slice(classOfIdent.pos.start, classOfIdent.pos.end).mkString
              val classOfRegex = """(?<=classOf\[)(.+)(?=\])""".r
              val classOfTypeParam = classOfRegex.findAllIn(realText).toList.headOption
              classOfTypeParam.filter { _ != classOfIdent.name.decoded }.map { _ =>
                ancestorSymbols(classOfIdent).filterNot { _.isPackageObject } match {
                  case x :: xs =>
                    val select = xs.foldLeft(Ident(x): Tree) {
                      case (inner, outer) => Select(inner, outer)
                    }
                    select.setType(value).setSymbol(value.sym).setPos(t.pos)
                  case Nil =>
                    classOfIdent
                }
              }.getOrElse(classOfIdent)
            case allOther => allOther
          }
          traverse(tree)
        case t: ApplyToImplicitArgs =>
          traverse(t.fun)
          t.args foreach traverse
      }

      private def annotations(fromTree: Tree) =
        if (fromTree.hasSymbol)
          fromTree.symbol.annotations
        else
          Nil

      override def traverse(tree: Tree): Unit = special.orElse {
        new ImplicitValDefTraverserPF[oi.type](oi)(self)
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
}

class NotPackageImportParticipants[O <: OrganizeImports](val organizeImportsInstance: O) {
  import organizeImportsInstance._
  import organizeImportsInstance.global._

  class RemoveUnused(block: Tree, addNewImports: List[(String, String)] = Nil) extends Participant {
    private lazy val allSelects = (new AllSelects[organizeImportsInstance.type](organizeImportsInstance))(block)
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

    def importSuppressesSelectType(select: Select, importSelector: ImportSelector) =
      (allTypeNamesFromSelect(select) & allTypeNamesFromImportSelector(importSelector)).nonEmpty &&
        importSelector.rename == nme.WILDCARD

    private def allTypeNamesFromSelect(select: Select) =
      Set(select.name.decoded, select.symbol.owner.nameString)

    private def allTypeNamesFromImportSelector(importSelector: ImportSelector) =
      Set(importSelector.name, importSelector.rename).collect {
        case name if name != null => name.decoded
      }

    private def isRelationBetweenTypesInSelectAndImportSelector(select: Select, importSelector: ImportSelector) =
      importSelector.name == nme.WILDCARD || (allTypeNamesFromSelect(select) & allTypeNamesFromImportSelector(importSelector)).nonEmpty

    import scala.tools.nsc.Global
    private def isSelectOwnerInImportOwner(selectOwner: Symbol, importOwner: Global#Symbol) =
      selectOwner.ownerChain.exists { owner =>
        owner.name.decoded == importOwner.name.decoded
      }

    private def isSelectExpressionPathASubpathOfImport(select: Select, importQualifier: Tree) =
      MiscTools.stableIdentifierSymbol(global)(select.qualifier.symbol).orElse {
        MiscTools.stableIdentifierSymbol(global)(select.symbol)
      }.map {
        (fullNameButSkipPackageObject _).andThen { _.startsWith(importSymbol(importQualifier)) }
      }.getOrElse(false)

    protected def doApply(trees: List[Import]) = trees.iterator.collect {
      case imp @ Import(importQualifier, importSelections) =>
        val impOwner = imp match {
          case ro: RegionOwner => Option(ro.owner)
          case _ => None
        }
        val usedSelectors = importSelections filter { importSel =>
          allSelects.exists { select =>
            val (owner, foundSel) = select
            isRelationBetweenTypesInSelectAndImportSelector(foundSel, importSel) &&
              impOwner.map { isSelectOwnerInImportOwner(owner, _) }.getOrElse(true) &&
              (isSelectExpressionPathASubpathOfImport(foundSel, importQualifier) ||
                importSuppressesSelectType(foundSel, importSel))
          } ||
            isScalaLanguageImport(imp) ||
            isInNewImports(imp)
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
    private val treeComparables = new TreeComparables[organizeImportsInstance.global.type](organizeImportsInstance.global)
    private def isSameExpr(leftExpr: Tree, rightExpr: Tree): Boolean =
      treeComparables.isSameWithSymbols(true)(leftExpr.symbol, rightExpr.symbol) || leftExpr.toString == rightExpr.toString

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

  object SortImports extends Participant {

    def asText(t: Tree) = createText(stripPositions(t))

    protected def doApply(trees: List[Import]) = {
      val AnyBeforeUppercase = "*"
      trees.sortBy {
        case i @ Import(expr, selector :: Nil) if !wildcardImport(selector) =>
          asText(expr) + "." + selector.name.toString
        case wildcard @ Import(expr, _ :: Nil) =>
          asText(expr) + "." + AnyBeforeUppercase
        case i @ Import(expr, selectors) =>
          asText(expr)
      }
    }
  }
}
