/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis
import scala.tools.nsc.util.RangePosition

trait CompilationUnitDependencies {
  // we need to interactive compiler because we work with RangePositions
  this: common.InteractiveScalaCompiler with common.TreeTraverser with common.TreeExtractors =>

  import global._

  /**
   * Calculates a list of all needed imports for the given Tree.
   */
  def neededImports(t: Tree): List[Select] = {

    def findLastVisibleSelect(t: Tree): Option[Select] = t match {
      case s @ Select(x, name) if s.pos.isRange && !x.pos.isRange =>
        x match {
          // we don't need to import anything that comes from the scala package
          case Ident(names.scala) => None
          case Select(Select(Ident(names.scala), names.pkg), _) => None
          case _ => Some(s)
        }
      case s: Select =>
        findLastVisibleSelect(s.qualifier)
      case _ =>
        None
    }
    
    val deps = dependencies(t)
    
    deps.flatMap {
      case t: Select if !t.pos.isRange => Some(t)
      case t => findLastVisibleSelect(t) 
    }.distinct
  }

  /**
   * Calculates all the external dependencies the given Tree has.
   * Compared to `neededImports`, this function might also return
   * trees that don't need to be explicitly imported.
   */
  def dependencies(t: Tree): List[Select] = {

    val result = new collection.mutable.HashMap[String, Select]

    val traverser = new Traverser {
      
      def isSelectFromInvisibleThis(t: Tree) = t.exists {
        case t: This => !t.pos.isRange
        case _ => false
      }
      
      def handleSelectFromImplicit(t: Tree) = t.find {
        case t: Select => !isSelectFromInvisibleThis(t)
        case _ => false
      } foreach {
        case Select(Ident(name), _) if name startsWith nme.EVIDENCE_PARAM_PREFIX=> 
          ()
        case t @ Select(qual, _) => 
          result += (t.toString -> t)
        case _ =>
          ()
      }
      
      override def traverse(root: Tree) = root match {

        case Import(_, _) => ()

        case Select(Ident(names.scala), _) => ()
        
        case Select(Select(Ident(names.scala), names.pkg), _) => ()
        
        case t : ApplyImplicitView =>
          handleSelectFromImplicit(t.fun)
          t.args foreach traverse
          
        case t : ApplyToImplicitArgs =>
          traverse(t.fun)
          t.args foreach handleSelectFromImplicit
        
        case Select(New(qual), _) =>
          traverse(qual)
        
        case t @ Select(qual, _) if t.pos.isRange =>
            
          // we don't need to add a dependency for method calls where the receiver
          // is explicit in the source code.
          val isMethodCallFromExplicitReceiver = qual.pos.isRange && t.symbol.isMethod
          
          if (!isMethodCallFromExplicitReceiver && !isSelectFromInvisibleThis(qual)) {
            result += (t.toString -> t)
          }

          super.traverse(t)

        case _ => super.traverse(root)
      }
    }

    traverser.traverse(t)
    
    val deps = result.values.toList

    deps.filterNot {
      case t => t.symbol.isPackage || t.symbol.isSynthetic
    } .toList
  }
}

