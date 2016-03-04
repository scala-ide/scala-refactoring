/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import tools.nsc.symtab.Flags
import scala.tools.refactoring.common.TracingImpl
import scala.tools.refactoring.common.PositionDebugging

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
      case ts: TermSymbol if ts.isVal && ts.owner.isClass && ts.pos.isRange =>
        declaration(s.owner).flatMap(findRelatedCtorParamSymbolIn(_, s))
      case _ => None
    }

    private def findRelatedCtorParamSymbolIn(parent: Tree, valSym: Symbol): Option[Symbol] = {
      parent.foreach {
        case dd: DefDef if dd.symbol.isConstructor =>
          def correspondsToVal(param: Tree) = {
            val (pSym, vSym) = (param.symbol, valSym)
            val (pPos, vPos) = (param.symbol.pos, valSym.pos)

            // Note that we intentionally look at 'start' only, because ends might differ
            // if default arguments are involved.
            pSym != vSym && pPos.start == vPos.start
          }

          val res = dd.vparamss.flatten.collectFirst {
            case p if correspondsToVal(p) => p.symbol
          }

          if (res.nonEmpty) {
            return res
          }

        case _ => ()
      }

      None
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
}
