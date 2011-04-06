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
  
  this: CompilerAccess with common.PimpedTrees =>
  
  import global._
  
  trait Traverser extends global.Traverser {
    override def traverse(t: Tree) = t match {
      // The standard traverser does not traverse a TypeTree's original:
      case t: TypeTree if t.original != null =>
        traverse(t.original)
      case t => 
        super.traverse(t)
    }
  }
  
  class FilterTreeTraverser(p: Tree => Boolean) extends global.FilterTreeTraverser(p) with Traverser

  def filterTree(t: Tree, traverser: FilterTreeTraverser) = {
    traverser.traverse(t)
    traverser.hits.toList
  }

  class TreeWithSymbolTraverser(f: (Symbol, Tree) => Unit) extends Traverser {
        
    override def traverse(t: Tree) = {
      t match {
        case t: TypeTree if t.original != null =>
    
          (t.original, t.tpe) match {
            case (att @ AppliedTypeTree(_, args1), tref @ TypeRef(_, _, args2)) =>

                args1 zip args2 foreach {
                  case (i: RefTree, tpe: TypeRef) => 
                    f(tpe.sym, i)
                  case _ => ()
                }
              case _ => ()
            }
            
        case t: DefTree if t.symbol != NoSymbol =>
          f(t.symbol, t)
        case t: RefTree =>
          f(t.symbol, t)
        case t: TypeTree =>
          
          def handleType(typ: Type): Unit = typ match {
            case RefinedType(parents, _) =>
              parents foreach handleType
            case TypeRef(_, sym, _) =>
              f(sym, t)
            case _ => ()
          }
          
          handleType(t.tpe)
          
        case t @ Import(expr, _) if expr.tpe != null =>
          
          def handleImport(iss: List[ImportSelectorTree], sym: Symbol): Unit = iss match {
            case Nil => 
              ()
            case (t @ ImportSelectorTree(NameTree(name), _)) :: _ if (name.toString == sym.name.toString)=> 
              f(sym, t)
            case _ :: rest => 
              handleImport(rest, sym)
          }
          
          expr.tpe.members foreach (handleImport(t.Selectors(), _))
          
        case _ => ()  
      }
        
      super.traverse(t)
    }
  }
}