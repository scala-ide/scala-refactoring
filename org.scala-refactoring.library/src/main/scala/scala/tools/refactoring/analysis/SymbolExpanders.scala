/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import tools.nsc.symtab.Flags

/**
 * Provides various traits that are used by the indexer
 * to expand symbols; that is, to find symbols that are
 * related to each other. For example, it finds overridden
 * methods in subclasses.
 */
trait DependentSymbolExpanders {

  this: Indexes with common.CompilerAccess =>

  import global._

  /**
   * The basic trait that is extended by the
   * concrete expanders.
   */
  trait SymbolExpander {
    def expand(s: Symbol): List[Symbol] = List(s)
  }

  trait ExpandGetterSetters extends SymbolExpander {
    abstract override def expand(s: Symbol) = super.expand(s) ++ (s match {
      case s if s.hasFlag(Flags.ACCESSOR) =>
        s.accessed :: Nil
      case s if s != NoSymbol && s.owner != NoSymbol =>
        s.getter(s.owner) :: s.setter(s.owner) :: Nil
      case _ =>
        Nil
    })
  }

  trait SuperConstructorParameters extends SymbolExpander {

    this: IndexLookup =>

    abstract override def expand(s: Symbol) = super.expand(s) ++ (s match {

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
    abstract override def expand(s: Symbol) = {
      s.companionSymbol :: super.expand(s)
    }
  }

  trait LazyValAccessor extends SymbolExpander {
    abstract override def expand(s: Symbol) = s match {
      case ts: TermSymbol if ts.isLazy =>
        ts.lazyAccessor :: super.expand(s)
      case _ =>
        super.expand(s)
    }
  }

  trait OverridesInClassHierarchy extends SymbolExpander {

    this: IndexLookup =>

    abstract override def expand(s: Symbol) = super.expand(s) ++ (s match {
      case s @ (_: global.TypeSymbol | _: global.TermSymbol) if s.owner.isClass => {

        def allSubClasses(clazz: Symbol) = allDefinedSymbols.filter {
          case decl if decl.isClass || decl.isModule =>
            val ancestors = decl.ancestors
            ancestors.exists { _ == clazz }
          case _ => false
        }
        val containingClass = s.owner

        val superClasses = containingClass.ancestors

        val subClasses = allSubClasses(containingClass)

        val overrides = (subClasses map (s overridingSymbol _)) ++ (superClasses map (s overriddenSymbol _))

        overrides
      }
      case _ => Nil
    })
  }

  trait SameSymbolPosition extends SymbolExpander {
    this: IndexLookup =>

    def sameSymbol(s1: Symbol, s2: Symbol) = {
      def sameFileName = {
        s1.pos.source.file.name == s2.pos.source.file.name
      }
      def sameRange = {
        s1.pos.sameRange(s2.pos)
      }
      def sameStringRep = {
        s1.toString == s2.toString
      }
      sameRange && sameFileName && sameStringRep
    }

    abstract override def expand(s: Symbol) = super.expand(s) ++ (allSymbols collect {
      case sym if sameSymbol(sym, s) && !sym.pos.isTransparent =>
        sym
    })
   }
}
