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
    
    def fakeSelectTreeFromTypeAndSymbol(tpe: Type, sym: Symbol, pos: Position) = {
      // we fake our own Select(Ident(..), ..) tree from the type so we
      // can handle them just like any other select call
    
      val stringRep = tpe.trimPrefix(tpe.toString)
            
      val select = stringRep.split("\\.").toList match {
        case x :: xs =>
          xs.foldLeft(Ident(x): Tree) {
            case (inner, outer) => Select(inner, outer)
          }
        case Nil => EmptyTree
      }
      select.setType(tpe).setSymbol(sym).setPos(pos)          
    }
    
    override def traverse(t: Tree) = {
      
      Option(t.symbol).toList flatMap (_.annotations) foreach { annotation =>
        annotation.atp match {
          case tpe @ TypeRef(_, sym, _) if annotation.pos != NoPosition =>
            val tree = fakeSelectTreeFromTypeAndSymbol(tpe, sym, annotation.pos)
            traverse(tree)
          case _ => 
        }
      }
      
      t match {
      
        case v: ValDef =>
          super.traverse(v)
        
        // The standard traverser does not traverse a TypeTree's original:
        case t: TypeTree if t.original != null =>
                
          def handleCompoundTypeTree(parents: List[Tree], parentTypes: List[Type]) = {
            parents zip parentTypes foreach {
              
              case (i @ Ident(name), tpe @ TypeRef(_, sym, _)) if i.tpe == null =>
                traverse(fakeSelectTreeFromTypeAndSymbol(tpe, sym, i.pos))
                
              case (tree, _) => traverse(tree)
            }
          }
  
          (t.tpe, t.original) match {
            // in a self type annotation, the first tree is the trait itself, so we skip that one
            
            // self type annotation with a single type
            case (RefinedType(_ :: tpe :: Nil, _), tree: Ident) =>
              handleCompoundTypeTree(List(tree), List(tpe))
              
            // self type annotation with a compound type
            case (RefinedType(_ :: RefinedType(parentTypes, _) :: Nil, _), CompoundTypeTree(Template(parents, self, body))) =>
              handleCompoundTypeTree(parents, parentTypes)

            // handle regular compound type trees
            case (RefinedType(parentTypes, _), CompoundTypeTree(Template(parents, self, body))) =>
              handleCompoundTypeTree(parents, parentTypes)
              
            case (tpe, SingletonTypeTree(ident)) if tpe != null => 
              tpe.widen match {
                case tpe @ TypeRef(_, sym, _) =>
                  traverse(fakeSelectTreeFromTypeAndSymbol(tpe, sym, ident.pos))
                case _ =>
                  traverse(t.original) 
              }
            case _ => 
              traverse(t.original)
          }
        case t => 
          super.traverse(t)
      }
    }
  }
  
  class FilterTreeTraverser(p: Tree => Boolean) extends global.FilterTreeTraverser(p) with Traverser

  def filterTree(t: Tree, traverser: global.FilterTreeTraverser) = {
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