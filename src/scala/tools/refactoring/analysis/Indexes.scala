/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.analysis

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Symbols

trait Indexes {

  val global: scala.tools.nsc.Global  
  import global._
  
  trait Index {
        
    def declaration(s: Symbol): DefTree
    
    def references (s: Symbol): List[SymTree]
    
    def occurences(s: Symbol): List[SymTree]
    
    def children(s: Symbol): List[DefTree]
  }
  
  def index: Index
}
