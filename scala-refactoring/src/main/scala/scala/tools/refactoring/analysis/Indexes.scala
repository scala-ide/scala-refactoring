/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols
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
     * Add more convenience functions here..
     * */
  }
  
  def index: IndexLookup
}
