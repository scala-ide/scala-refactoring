/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import common.PimpedTrees

/**
 * Provides an implementation of the Indexes.IndexLookup trait
 * by combining various CompilationUnitIndexes. Note that creating
 * the GlobalIndex is cheap, all the compilation units were already
 * indexed, and all further work is only done on demand.
 * 
 */
trait GlobalIndexes extends Indexes with DependentSymbolExpanders with CompilationUnitIndexes with common.PimpedTrees with common.InteractiveScalaCompiler {

  import global._
  
  object GlobalIndex {
    
    def apply(compilationUnits: List[CompilationUnitIndex]): IndexLookup =
      new GlobalIndex with
          ExpandGetterSetters with
          SuperConstructorParameters with
          Companion with
          SameSymbolPosition with
          OverridesInClassHierarchy {
        val cus = compilationUnits
      }
    
    def apply(t: Tree): IndexLookup = apply(List(CompilationUnitIndex(t)))
  }
  
  trait GlobalIndex extends IndexLookup {
    
    this: SymbolExpander =>
    
    def cus(): List[CompilationUnitIndex]
    
    def declaration(s: Symbol): Option[DefTree] = 
      cus.flatMap(_.definitions.get(s)).flatten.headOption
   
    def references(s: Symbol) = {
      val decls = declaration(s).toList
      val occs = occurences(s) 
      occs filterNot decls.contains
    }
    
    def expandSymbol(s: global.Symbol): List[global.Symbol] = {
      def expandSymbols(ss: List[Symbol]) = ss flatMap expand filter (_ != NoSymbol) distinct
            
      // we should probably do this until a fixpoint is reached. but for now, three times seems to be enough
      expandSymbols(expandSymbols(expandSymbols(List(s))))
    }
      
    def occurences(s: global.Symbol) = {
      expandSymbol(s) flatMap { sym =>
        declaration(sym).toList ::: cus.flatMap { cu => 
            cu.references.get(sym).flatten
          }
      } filter (_.pos.isRange) distinct
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
      } filter (_ != NoSymbol) distinct
    }
  }
}
