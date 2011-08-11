/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

import collection.mutable.{HashMap, ListBuffer}

/**
 * A CompilationUnitIndex is a light-weight index that
 * holds all definitions and references in a compilation
 * unit. This index is built with the companion object, 
 * which traverses the whole compilation unit once and
 * then memoizes all relations.
 * 
 */
trait CompilationUnitIndexes {
  
  this: common.PimpedTrees with common.CompilerAccess with common.TreeTraverser =>
  
  import global._
  
  trait CompilationUnitIndex {
    def definitions: Map[Symbol, List[DefTree]]
    def references:  Map[Symbol, List[Tree]]
  }
  
  object CompilationUnitIndex {
  
    def apply(tree: Tree) = {
      
      val defs = new HashMap[Symbol, ListBuffer[DefTree]]
      val refs = new HashMap[Symbol, ListBuffer[Tree]]
  
      def addDefinition(s: Symbol, t: DefTree) {
        def add(s: Symbol) = 
          defs.getOrElseUpdate(s, new ListBuffer[DefTree]) += t
        
        t.symbol match {
          case ts: TermSymbol if ts.isLazy =>
            add(ts.lazyAccessor)
          case _ =>
            add(s)
        }
      }

      def addReference(s: Symbol, t: Tree) {
        def add(s: Symbol) = 
          refs.getOrElseUpdate(s, new ListBuffer[Tree]) += t

        add(s)
        
        s match {
          case _: ClassSymbol => ()
          /*
           * If we only have a TypeSymbol, we check if it is 
           * a reference to another symbol and add this to the
           * index as well.
           * 
           * This is needed for example to find the TypeTree
           * of a DefDef parameter-ValDef
           * */
          case ts: TypeSymbol =>
            ts.info match {
              case tr: TypeRef if tr.sym != null =>
                add(tr.sym)
              case _ => ()
            }
          case _ => ()
        }
      }
      
      def handleSymbol(s: Symbol, t: Tree) = t match {
        case t: DefTree => addDefinition(s, t)
        case _ => addReference(s, t)
      }      
      
      (new TreeWithSymbolTraverser(handleSymbol)).traverse(tree)
      
      new CompilationUnitIndex {
        val definitions = defs.map {case (k, v) => k → v.toList} toMap
        val references = refs.map {case (k, v) => k → v.toList} toMap
      }
    }
  }
}