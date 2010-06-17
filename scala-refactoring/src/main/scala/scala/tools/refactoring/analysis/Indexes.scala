/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package analysis

/**
 * The Indexes trait is mixed in by refactorings that need an index.
 * It provides several lookup functions to find references and decl-
 * arations of symbols.
 * 
 * The IndexLookup trait has been separated into two traits: the
 * TrivialIndexLookup simply gives access to the underlying data,
 * whereas the IndexLookup that is used by clients contains more
 * expensive operations.
 * 
 * An implementation can be found in IndexImplementations.
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
     * either referenced or defined.
     * */
    def allSymbols(): List[global.Symbol]    
    
    /**
     * For a given Symbol, tries to find the tree that declares it.
     * The result tree can have an offset position.
     * */
    def declaration(s: global.Symbol): Option[global.DefTree]
    
    /**
     * For a given Symbol, returns all trees that contain a reference
     * to it. Only returns trees with a range position.
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
      s :: (allDefinedSymbols filter (_.ancestors contains s) flatMap (s => s :: s.ancestors)) filter (_.pos != global.NoPosition) distinct
    
    /**
     * For the given Symbol - which is a package - returns a
     * list of all sub- and super packages, in no particular order.
     */
    def completePackageHierarchy(s: global.Symbol): List[global.Symbol] =
      allDefinedSymbols filter (_.ownerChain contains s) flatMap (s => s :: s.ownerChain) filter (_.isPackageClass) filter (_.pos != global.NoPosition) distinct
      
    /**
     * Add more convenience functions here..
     * */
  }
  
  def index: IndexLookup
}
