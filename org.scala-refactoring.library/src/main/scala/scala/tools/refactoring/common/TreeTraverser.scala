/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile
import collection.mutable.ListBuffer
import tools.nsc.Global
import tools.nsc.util.RangePosition

trait TreeTraverser {
  
  this: CompilerAccess =>
  
  class Traverser extends global.Traverser {
    override def traverse(t: global.Tree) = t match {
      case t: global.TypeTree if t.original != null =>
        traverse(t.original)
      case t => 
        super.traverse(t)
    }
  }
  
  class FilterTreeTraverser(p: global.Tree => Boolean) extends Traverser {
    val hits = new ListBuffer[global.Tree]
    override def traverse(t: global.Tree) {
      if (p(t)) hits += t
      super.traverse(t)
    }
  }
}