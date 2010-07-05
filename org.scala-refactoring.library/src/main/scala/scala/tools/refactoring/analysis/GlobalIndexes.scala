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
 * */
trait GlobalIndexes extends Indexes with DependentSymbolExpanders with CompilationUnitIndexes with PimpedTrees {

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
      occurences(s) filterNot decls.contains
    }
      
    def occurences(s: global.Symbol) = {
      
      def expandSymbols(ss: List[Symbol]) = ss flatMap expand filter (_ != NoSymbol) distinct
      
      // we should probably do this until a fixpoint is reached. but for now, three times seems to be enough
      val expanded = expandSymbols(expandSymbols(expandSymbols(List(s))))

      expanded flatMap { sym =>
        declaration(sym).toList ::: cus.flatMap(_.references.get(sym).flatten)
      } filter (_.pos.isRange) distinct
    }
    
    def allDefinedSymbols = cus.flatMap(_.definitions.keys)
    
    def allSymbols = cus.flatMap(cu => cu.definitions.keys ++ cu.references.keys)
  }
}
