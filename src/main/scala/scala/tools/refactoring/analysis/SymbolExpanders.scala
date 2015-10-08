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
          if (res.map(_.pos).toSet == Set(NoPosition)) Seq()
          else if (res == Seq(s)) Seq()
          else res.map(elem => PositionDebugging.formatCompact(elem.pos))
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
      case s if s != NoSymbol && s.owner != NoSymbol =>
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
