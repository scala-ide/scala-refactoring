/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import tools.nsc.symtab.Flags
import scala.tools.refactoring.common.TracingImpl
import scala.tools.refactoring.common.PositionDebugging
import scala.reflect.internal.util.RangePosition

/**
 * Provides various traits that are used by the indexer
 * to expand symbols; that is, to find symbols that are
 * related to each other. For example, it finds overridden
 * methods in subclasses.
 */
trait DependentSymbolExpanders extends TracingImpl {
  this: Indexes with common.CompilerAccess =>

  import global._

  /**
   * The basic trait that is extended by the
   * concrete expanders.
   */
  trait SymbolExpander {
    final def expand(s: Symbol): List[Symbol] = {
      doExpand(s).filterNot(_ == NoSymbol) \\ { res =>
        val debugInfo = {
          val posSet = res.map(_.pos).toSet

          if (posSet == Set(NoPosition)) Seq()
          else if (res == Seq(s)) Seq()
          else posSet.map(pos => PositionDebugging.formatCompact(pos))
        }

        if (debugInfo.nonEmpty) {
          trace("Expanding %s to %s", s.nameString,  debugInfo)
        }
      }
    }

    protected def doExpand(s: Symbol): List[Symbol] = List(s)
  }

  trait ExpandGetterSetters extends SymbolExpander {
    this: IndexLookup =>

    protected abstract override def doExpand(s: Symbol) = super.doExpand(s) ++ (s match {
      case s if s.hasFlag(Flags.ACCESSOR) =>
        s.accessed :: Nil
      case s if s != NoSymbol && s.owner != NoSymbol && !s.isLocal =>
        (s.getter(s.owner) :: s.setter(s.owner) :: Nil)
      case _ =>
        Nil
    })
  }

  trait SuperConstructorParameters extends SymbolExpander {

    this: IndexLookup =>

    protected abstract override def doExpand(s: Symbol) = super.doExpand(s) ++ (s match {

      case s if s != NoSymbol && s.owner.isClass && s.hasFlag(Flags.ACCESSOR) =>

        (declaration(s.owner) collect {
          case ClassDef(_, _, _, Template(_, _, body)) => body.collect {
            case d @ DefDef(_, _, _, _, _, Block(stats, _)) if d.symbol.isConstructor => stats.collect {
              case Apply(_, args) => args collect {
                case symTree: SymTree if symTree.symbol.nameString == s.nameString => symTree.symbol
              }
            }.flatten
          }.flatten
        }).toList.flatten

    case _ => Nil
    })
  }

  trait Companion extends SymbolExpander {
    protected abstract override def doExpand(s: Symbol) = {
      s.companionSymbol :: super.doExpand(s)
    }
  }

  trait LazyValAccessor extends SymbolExpander {
    protected abstract override def doExpand(s: Symbol) = s match {
      case ts: TermSymbol if ts.isLazy =>
        ts.lazyAccessor :: super.doExpand(s)
      case _ =>
        super.doExpand(s)
    }
  }

  /**
   * Associates class vals with constructor parameters
   */
  trait ClassVals extends SymbolExpander { this: IndexLookup =>
    protected abstract override def doExpand(s: Symbol) = {
      findRelatedCtorParamSymbol(s).toList ::: super.doExpand(s)
    }

    private def findRelatedCtorParamSymbol(s: Symbol): Option[Symbol] = s match {
      case ts: TermSymbol if ts.isVal && ts.owner.isClass =>
        declaration(s.owner).flatMap(findRelatedCtorParamSymbolIn(_, s))
      case _ => None
    }

    private def findRelatedCtorParamSymbolIn(parent: Tree, valSym: Symbol): Option[Symbol] = {
      parent.foreach {
        case dd: DefDef if dd.symbol.isConstructor =>
          val res = findParamSymbolAssociatedWithValSymbol(dd, valSym)

          if (res.nonEmpty) {
            return res
          }

        case _ => ()
      }

      None
    }
  }

  private def findParamSymbolAssociatedWithValSymbol(dd: DefDef, valSym: Symbol): Option[Symbol] = {
    def correspondsToVal(param: Tree) = {
      val (pSym, vSym) = (param.symbol, valSym)
      val (pPos, vPos) = (param.symbol.pos, valSym.pos)
      pSym != vSym && pPos.isDefined && vPos.isDefined && pPos.point == vPos.point
    }

    dd.vparamss.flatten.collectFirst {
      case p if correspondsToVal(p) => p.symbol
    }
  }

  /**
   * Associates case class vals with the parameters of generated apply and copy methods
   */
  trait CaseClassVals extends SymbolExpander { this: IndexLookup =>
    protected abstract override def doExpand(s: Symbol) = {
      findRelatedApplyAndCopyParamSymbols(s) ::: super.doExpand(s)
    }

    /*
     * Unfortunately we cannot rely on `Symbol.companionSymbol` alone (see Scaladocs).
     * The implementation below is heavily inspired by
     * `scala.tools.nsc.typechecker.Namers.companionSymbolOf`.
     */
    private def companionModuleOf(classSymbol: Symbol): Symbol = {
      classSymbol.companionSymbol.orElse {
        val companionName = classSymbol.name.companionName

        allDefinedSymbols().find { sym =>
          sym.name == companionName && sym.hasModuleFlag && sym.isCoDefinedWith(classSymbol)
        }.getOrElse(NoSymbol)
      }
    }

    private def findRelatedApplyAndCopyParamSymbols(s: Symbol): List[Symbol] = {
      if (s.isVal && s.owner.isCaseClass) {
        List(s.owner, companionModuleOf(s.owner)).flatMap { parentSymbol =>
          declaration(parentSymbol).flatMap(findRelatedApplyOrCopyParamSymbolIn(_, s))
        }
      } else {
        Nil
      }
    }

    private def findRelatedApplyOrCopyParamSymbolIn(parent: Tree, valSym: Symbol): Option[Symbol] = {
      parent.foreach {
        case dd: DefDef if isCaseApplyOrCopy(dd.symbol) =>
          return findParamSymbolAssociatedWithValSymbol(dd, valSym)
        case _ => ()
      }

      None
    }

    private def isCaseApplyOrCopy(s: Symbol) = {
      /*
       *  Unfortunately Symbol.isCaseCopy is not available for Scala-2.10
       */
      def isCaseCopy = {
        s.isMethod && s.owner.isCase && s.isSynthetic && s.name == nme.copy
      }

      def isCaseApply = {
        s.isCaseApplyOrUnapply && !s.nameString.contains("unapply")
      }

      isCaseCopy || isCaseApply
    }
  }

  trait OverridesInSuperClasses extends SymbolExpander {
    this : IndexLookup =>

    protected abstract override def doExpand(s: Symbol): List[Symbol] = super.doExpand(s) ++ (s match {
      case s @ (_: global.TypeSymbol | _: global.TermSymbol) if s.owner.isClass => {
        s.allOverriddenSymbols
      }
      case _ => Nil
    })
  }

  /**
   * Associates term symbols with missing ranges to related symbols that have ranges.
   *
   * The reason that we need this is that in some cases, the PC generates multiple
   * symbols for one and the same symbol in user source code, one of them with a
   * proper range position, and others just with offset positions. One place where
   * this happens is in desugared for comprehensions with filter clauses.
   * See Assembler Ticket #1002650.
   */
  trait TermsWithMissingRanges extends SymbolExpander { this: IndexLookup =>
    protected abstract override def doExpand(s: Symbol): List[Symbol] = {
      termsWithSamePointButRange(s) ::: super.doExpand(s)
    }

    private def termsWithSamePointButRange(s: Symbol): List[Symbol] = {
      val lookAtSymbol = {
        s != NoSymbol && !s.isSynthetic && s.isTerm && !s.pos.isRange && s.pos.isDefined
      }

      if (!lookAtSymbol) {
        Nil
      } else {
        allDefinedSymbols().filter { ds =>
          ds.pos match {
            case rp: RangePosition => rp.point == s.pos.point && ds.nameString == s.nameString
            case _ => false
          }
        }
      }
    }
  }
}
