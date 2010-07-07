/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

/**
 * The Indexes trait is mixed in by refactorings that need an index.
 * It provides several lookup functions to find references and decla-
 * rations of symbols.
 * 
 * The IndexLookup trait has been separated into two traits: the
 * TrivialIndexLookup simply gives access to the underlying data,
 * whereas the IndexLookup that is used by clients contains more
 * expensive operations.
 * 
 * An implementation can be found in GlobalIndexes.
 * */
trait Indexes {

  val global: scala.tools.nsc.interactive.Global
  
  trait IndexLookup {
    
    /**
     * Returns all defined symbols, i.e. symbols
     * of DefTrees.
     * */
    def allDefinedSymbols(): List[global.Symbol]
    
    /**
     * Returns all symbols that are part of the index,
     * either referenced or defined. This also includes
     * symbols from the Scala library that are used
     * in the compilation units.
     * */
    def allSymbols(): List[global.Symbol]    
    
    /**
     * For a given Symbol, tries to find the tree that declares it.
     * The result tree can have an offset position.
     * */
    def declaration(s: global.Symbol): Option[global.DefTree]
    
    /**
     * For a given Symbol, returns all trees that directly 
     * reference the symbol. This does not include parents
     * of trees that reference a symbol, e.g. for a method
     * call, the Select tree is returned, but not its parent
     * Apply tree. 
     * 
     * Only returns trees with a range position.
     * */
    def references(s: global.Symbol): List[global.Tree]
    
    /**
     * For a given Symbol, returns all trees that reference or
     * declare the symbol that have a range position.
     * */
    def occurences(s: global.Symbol): List[global.Tree]

    /**
     * For the given Symbol - which is a class or object - returns a
     * list of all sub- and super classes, in no particular order.
     */
    def completeClassHierarchy(s: global.Symbol): List[global.Symbol] =
      s :: (allDefinedSymbols filter (_.ancestors contains s)) flatMap (s => s :: s.ancestors) filter (_.pos != global.NoPosition) distinct
    
    /**
     * For the given Symbol - which is a package - returns a
     * list of all sub- and super packages, in no particular order.
     */
    def completePackageHierarchy(s: global.Symbol): List[global.Symbol] =
      allDefinedSymbols filter (_.ownerChain contains s) flatMap (s => s :: s.ownerChain) filter (_.isPackageClass) filter (_.pos != global.NoPosition) distinct
      
    /**
     * Returns a map that associates each defined symbol in the index
     * with its DefTree.
     */
    def allDeclarations(): Map[global.Symbol, global.DefTree] = 
      allDefinedSymbols() flatMap (sym => declaration(sym) map (sym → _)) toMap
      
    /**
     * Returns all overrides of the symbol s.
     */
    def overridesInClasses(s: global.Symbol): List[global.Symbol] =
      completeClassHierarchy(s.owner) map s.overridingSymbol  filterNot global.NoSymbol.==
    
    /**
     * Add more convenience functions here..
     * */
  }
  
  def index: IndexLookup
}
