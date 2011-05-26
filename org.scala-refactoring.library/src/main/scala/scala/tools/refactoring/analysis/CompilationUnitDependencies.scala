/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package analysis

trait CompilationUnitDependencies {
  // we need to interactive compiler because we work with RangePositions
  this: common.InteractiveScalaCompiler with common.TreeTraverser =>

  import global._

  /**
   * Calculates a list of all needed imports for the given Tree.
   */
  def neededImports(t: Tree): List[Select] = {

    def findLastVisibleSelect(t: Tree): Option[Select] = t match {
      case s @ Select(x, name) if s.pos.isRange && !x.pos.isRange =>
        x match {
          // we don't need to import anything that comes from the scala package
          case Ident(name) if name.toString == "scala" => None
          case _ => Some(s)
        }
      case s: Select =>
        findLastVisibleSelect(s.qualifier)
      case _ =>
        None
    }
    
    dependencies(t).flatMap {
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
        case t: Select => result += (t.toString -> t)
        case _ => 
      }
      
      override def traverse(t: Tree) = t match {

        case Import(_, _) => ()

        case Select(Ident(name), _) if name.toString == "scala" => ()
        
        case Select(New(qual), _) => traverse(qual)
        
        case t : ApplyImplicitView =>
          handleSelectFromImplicit(t.fun)
          t.args foreach super.traverse
          
        case t : ApplyToImplicitArgs =>
          super.traverse(t.fun)
          t.args foreach handleSelectFromImplicit

        case t @ Select(qual, _) if t.pos.isRange =>
          
          if (!isSelectFromInvisibleThis(qual)) {
            result += (t.toString -> t)
          }

          super.traverse(t)

        case _ => super.traverse(t)
      }
    }

    traverser.traverse(t)

    result.values.filterNot(s => s.symbol.isPackage).toList
  }
}

