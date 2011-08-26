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
  
  /**
   *  A traverser that also traverses a TypeTree's original type.
   */
  trait Traverser extends global.Traverser {
    override def traverse(t: Tree) = t match {
      case t: TypeTree if t.original != null => 
        traverse(t.original)
      case t => 
        super.traverse(t)
     }
  }
  
  /**
   *  A traverser that creates fake trees for various
   *  type trees so they can be treated as if they were
   *  regular trees.
   */
  trait TraverserWithFakedTrees extends global.Traverser {
    
    def fakeSelectTreeFromType(tpe: Type, sym: Symbol, pos: Position) = {
      // we fake our own Select(Ident(..), ..) tree from the type
      // so we can handle them just like any other select call
    
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
    
    def fakeSelectTree(tpe: Type, sym: Symbol, tree: Tree): Tree = {
    
      val flattenedExistingTrees = tree.filter(_ => true) map {
        case t: Ident =>  (t.name.toString, t.pos)
        case t: Select => (t.name.toString, t.pos)
        case _ => return tree
      }
      
      val treesFromType = tpe.trimPrefix(tpe.toString).split("\\.").toList.reverse zip Stream.continually(NoPosition)
      
      val fullPathOfAllTrees = (flattenedExistingTrees ++ treesFromType.drop(flattenedExistingTrees.size)).reverse
      
      def symbolAncestors(s: Symbol): Stream[Symbol] = if(s == NoSymbol) Stream.continually(NoSymbol) else Stream.cons(s, symbolAncestors(s.owner))
      
      val select = fullPathOfAllTrees zip symbolAncestors(sym).take(fullPathOfAllTrees.length).reverse.toList match {
        case ((x: String, pos: Position), sym: Symbol) :: xs =>
          val i = Ident(x).setPos(pos).setSymbol(sym)
          xs.foldLeft(i: Tree) {
            case (inner: Ident, ((outer, pos), sym)) if outer == "this" => 
              new This(inner.name.toTypeName).setPos(pos).setSymbol(sym)
            case (inner: Ident, ((outer, pos), sym)) if outer == "package" => 
              inner
            case (inner, ((outer, pos), sym)) => 
              Select(inner, outer).setPos(pos).setSymbol(sym)
          }
        case Nil => EmptyTree
      }
      
      select.setType(tpe)
      
      select
    }
    
    def handleAnnotations(as: List[AnnotationInfo]) {
      as foreach { annotation =>
        annotation.atp match {
          case tpe @ TypeRef(_, sym, _) if annotation.pos != NoPosition =>
            val tree = fakeSelectTreeFromType(tpe, sym, annotation.pos)
            traverse(tree)
          case _ => 
        }
      }
    }
    
    def handleAppliedTypeTree(tree: AppliedTypeTree, tpe: TypeRef): Unit = (tree, tpe) match {
      case (tree @ AppliedTypeTree(tpt, treeArgs), tpe @ TypeRef(_, sym, tpeArgs)) =>   

        val t = fakeSelectTree(sym.tpe, sym, tpt)
        traverse(t)
        
        tpeArgs zip treeArgs foreach {
          case (tpe: TypeRef, tree: AppliedTypeTree) =>
            handleAppliedTypeTree(tree, tpe)
          case (TypeRef(_, sym, _), tree) =>
            fakeSelectTree(sym.tpe, sym, tree) foreach traverse
        }
    }
    
    override def traverse(t: Tree) = {
      
      Option(t.symbol) foreach (s => handleAnnotations(s.annotations))
      
      t match {
        // The standard traverser does not traverse a TypeTree's original:
        case t: TypeTree if t.original != null =>
                
          def handleCompoundTypeTree(parents: List[Tree], parentTypes: List[Type]) = {
            parents zip parentTypes foreach {
              
              case (i @ Ident(name), tpe @ TypeRef(_, sym, _)) if i.tpe == null =>
                fakeSelectTree(tpe, sym, i) foreach traverse
                
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
              
            case (tpe, SingletonTypeTree(tree)) if tpe != null => 
              tpe.widen match {
                case tpe @ TypeRef(_, sym, _) =>
                  fakeSelectTree(tpe, sym, tree) foreach traverse
                case _ =>
                  traverse(t.original) 
              }
              
            case (tpe: TypeRef, tpt: AppliedTypeTree) if tpe != null => 
              handleAppliedTypeTree(tpt, tpe)
              traverse(tpt)
              
            case (AnnotatedType(annotations, underlying, selfsym), _) =>
              handleAnnotations(annotations)
              traverse(t.original)
              
            case (ExistentialType(quantified, TypeRef(_, sym, _)), ExistentialTypeTree(AppliedTypeTree(tpt, _), _)) =>
              fakeSelectTree(sym.tpe, sym, tpt) foreach traverse
              
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
            
        case t: ClassDef if t.symbol != NoSymbol => 
          
          /* Class parameters passed to super constructors:
           * 
           *  class Sub(a: String) extends Base(a) 
           * 
           * are actually represented like this:
           * 
           *  class Sub extends Base {
           *    <paramaccessor> private[this] val a: String = _
           *    def this(a: String) = {
           *      Sub.super.this(a)
           *    }
           *  }
           *   
           * So we need to manually create the link between the
           * paramaccessor and the argument to super: 
           * */
          
          val superConstrArgs = t.impl.superConstructorParameters
          
          superConstrArgs foreach { superArg =>

            val constrParamAccessors = t.symbol.constrParamAccessors
            
            Option(superArg.symbol) foreach { superArgSymbol =>
              constrParamAccessors.find(_.name == superArgSymbol.name) foreach { sym =>
                f(sym, superArg)
              }

              val constructorParameters = t.impl.constructorParameters
              
              constructorParameters.find(_.name == superArgSymbol.name) foreach { t =>
                // we also add a reference from the super argument's symbol to the 
                // constructor parameter accessor
                f(superArgSymbol, t)
              }
              
            }
          }
          
          f(t.symbol, t)
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
          
        case t @ Import(expr, selectors) if expr.tpe != null =>

          t.Selectors() foreach { selector =>
            findSymbolForImportSelector(expr, selector.name.name) foreach { sym =>
              f(sym, selector)
            }
          }

        case _ => ()  
      }
        
      super.traverse(t)
    }
  }
}