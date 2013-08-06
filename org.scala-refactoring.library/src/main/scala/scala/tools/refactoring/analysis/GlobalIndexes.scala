/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import common.PimpedTrees
import collection.mutable.ListBuffer
import collection.mutable.HashSet
import annotation.tailrec

/**
 * Provides an implementation of the Indexes.IndexLookup trait
 * by combining various CompilationUnitIndexes. Note that creating
 * the GlobalIndex is cheap, all the compilation units were already
 * indexed, and all further work is only done on demand.
 *
 */
trait GlobalIndexes extends Indexes with DependentSymbolExpanders with CompilationUnitIndexes with common.PimpedTrees with common.InteractiveScalaCompiler with common.TreeTraverser {

  import global._

  object GlobalIndex {

    def apply(compilationUnits: List[CompilationUnitIndex]): IndexLookup =
      new GlobalIndex with
          ExpandGetterSetters with
          SuperConstructorParameters with
          Companion with
          LazyValAccessor with
          OverridesInClassHierarchy with
          SameSymbolPosition {
        val cus = compilationUnits
      }

    def apply(t: Tree): IndexLookup = apply(List(CompilationUnitIndex(t)))
  }

  val EmptyIndex = GlobalIndex(Nil)

  trait GlobalIndex extends IndexLookup {

    this: SymbolExpander =>

    def cus(): List[CompilationUnitIndex]

    def declaration(s: Symbol): Option[DefTree] = {
      cus.flatMap(_.definitions.get(s)).flatten.headOption
    }

    def references(s: Symbol) = {
      val decls = declaration(s).toList
      val occs = occurences(s)
      occs filterNot decls.contains
    }

    def rootsOf(trees: List[Tree]) = {
      (for {
        cu <- cus
        tree <- trees
        if cu.root.pos.source.file == tree.pos.source.file
      } yield {
        cu.root
      }).distinct
    }

    def expandSymbol(s: global.Symbol): List[global.Symbol] = {

      @tailrec
      def expandSymbols(res:ListBuffer[Symbol], seen: HashSet[Symbol], ss: List[Symbol], n:Int):List[Symbol] =
        if (n == 0) res.toList else{
          val nuS = ss flatMap expand filterNot (x => seen(x) || x == NoSymbol)
          if (nuS.isEmpty) res.toList else {
            nuS foreach { s =>
              seen += s
              res += s
              }
            expandSymbols(res, seen, nuS, n-1)
            }
          }

      // we should probably do this until a fixpoint is reached. but for now, three times seems to be enough
      expandSymbols(ListBuffer(s), HashSet(s), List(s), 3)
    }

    def occurences(s: global.Symbol) = {
      val expandedSymbol = expandSymbol(s)

      expandedSymbol.flatMap { sym =>

        val decs = declaration(sym).toList
        val refs = cus.flatMap { cu =>
          cu.references.get(sym).toList.flatten
        }

        decs ::: refs

      }.filter {

        // see SI-6141
        case t: Ident => t.pos.isRange

        // Calls to super.xy are This trees with a ClassSymbol, but we don't
        // want to see them when looking for occurrences of the ClassSymbol,
        // so we filter them here.
        case t: This if t.pos.isRange =>
          val src = t.pos.source.content.slice(t.pos.start, t.pos.end).mkString
          src != "super"

        case t => t.pos.isOpaqueRange
      }.distinct
    }

    def allDefinedSymbols = cus.flatMap(_.definitions.keys)

    def allSymbols = cus.flatMap(cu => cu.definitions.keys ++ cu.references.keys)

    def positionToSymbol(p: global.Position): List[global.Symbol] = {

      val hasTreeWithPos: Pair[global.Symbol, List[global.Tree]] => List[global.Symbol] = {
        case (sym, trees) if trees.exists(_.pos == p) =>
            List(sym)
        case _ =>
            Nil
      }

      cus.flatMap { cu =>
        cu.definitions.flatMap(hasTreeWithPos) ++ cu.references.flatMap(hasTreeWithPos)
      }.filter(_ != NoSymbol).distinct
    }
  }
}
