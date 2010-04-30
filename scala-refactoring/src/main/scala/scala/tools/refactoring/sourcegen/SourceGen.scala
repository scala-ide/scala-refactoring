package scala.tools.refactoring
package sourcegen

import common.{Change, PimpedTrees, Tracing}

trait SourceGen extends PrettyPrinter with PimpedTrees {
  
  self: LayoutHelper with Tracing =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
 
  val transformations = new Transformations[Tree]
  import transformations._
  
  def originalLayout()(implicit t: Tree): List[(Layout, Layout)] = {
    
    val originalTree = findOriginalTree(t).get //exception is ok for now
    
    val children = (originalTree match {
      
      // don't forget the mods!
      
      case t @ PackageDef(pid, stats) => 
        pid :: stats
      
      case t @ ClassDef(mods, name, tparams, impl) =>
        NameTree(name, t.namePosition) :: tparams ::: impl :: Nil
        
      case t @ Template(parents, self, body) =>
        parents ::: self :: body
        
      case t @ ValDef(mods, name, tpt, rhs) =>
       NameTree(name, t.namePosition) :: tpt :: rhs :: Nil
       
      case t => Nil
    }) filterNot (_.pos == NoPosition)

    val a = splitLayoutBetweenParentAndFirstChild(parent = originalTree, child = children.head)
    val c = splitLayoutBetweenLastChildAndParent(child = children.last, parent = originalTree)
    
    a :: c :: Nil

  }
  
  def printWithExistingLayout(f: Transformation[Tree, String], left: String, t: Tree, right: String) = {
    
    implicit val currentTree = t
    implicit val currentFile = t.pos.source
    
    val originalTree = findOriginalTree(t).get //exception is ok for now
    
    trace("leading:  %s", left)
    trace("trailing: %s", right)
    
    val center = (t, originalTree) match {
      
      // we can solve it generic for all trees that have only subtrees but no names, etc.
      
      case (t @ PackageDef(pid, stats), _) =>
        val o = originalLayout()
        o.head._1 + f(pid).get + (stats map f map (_.get) mkString) + o.last._2
      
      case (t @ ClassDef(mods, name, tparams, impl), _) =>
        val o = originalLayout()
        o.head._1 + name.toString + f(impl).get + o.last._2
        
      case (t @ Template(parents, self, Nil), _) =>  
        val o = originalLayout() 
        o.head._1.toString + o.last._2.toString
        
      case (t @ Template(parents, self, body), orig: Template) =>
        val o = originalLayout() 
        
        val b = (body map f map (_.get))
        
        o.head._1 + b.mkString + o.last._2
        
      case (t @ ValDef(ModifiersTree(mods), newName, EmptyTree, rhs), orig @ ValDef(ModifiersTree(modsOrig), _, _, rhsOrig)) =>
        
        val nameOrig = NameTree(orig.name, orig.namePosition)
        
        val a = splitLayoutBetweenSiblings(mods.last, nameOrig)._1
        val b = splitLayoutBetweenSiblings(nameOrig, rhsOrig)._1
        
        if(mods zip modsOrig exists (x => x._1.pos != x._2.pos)) {
          // handle changed modifiers
          "mods != modsOrig"
        } else { 
          layout(mods.head.pos.start, mods.last.pos.end).toString + a + newName.toString.trim + b + f(rhs).getOrElse("")
        }
             
      case (t @ ValDef(ModifiersTree(mods), newName, tpt, rhs), orig @ ValDef(ModifiersTree(modsOrig), _, tptOrig, rhsOrig)) =>
        
        val nameOrig = NameTree(orig.name, orig.namePosition)
        
        val a = splitLayoutBetweenSiblings(mods.last, nameOrig)._1
        val b = splitLayoutBetweenSiblings(nameOrig, tptOrig)
        
        if(mods zip modsOrig exists (x => x._1.pos != x._2.pos)) {
          // handle changed modifiers
          "mods != modsOrig"
        } else { 
          layout(mods.head.pos.start, mods.last.pos.end).toString + a + newName.toString.trim + b._2 + f(tpt).getOrElse("") + f(rhs).getOrElse("")
        }
          
      case (t : TypeTree, _) => t.toString
        
      case (t : Ident, _) => t.name
      
      case (t : Literal, _) => t.toString
        
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        "ø"
      
    }
    
    trace("result: %s", left + center + right )
    
    left + center + right 
  }
  
  def reuseExistingSource(traverse: Transformation[Tree, String], t: Tree): String = context("Reuse "+ t.getClass.getSimpleName) { 
    

    val (leftLayout, rightLayout) = (findOriginalTree(t) map { t =>

      val (leftLayout, rightLayout) = (t.originalLeftSibling, t.originalParent, t.originalRightSibling) match {
        case (_,          None,    _          ) => layoutForCuRoot(t)
        case (None,       Some(p), None       ) => layoutForSingleChild(t, p)
        case (None,       Some(p), Some(right)) => layoutForLeftOuterChild(t, p, right)
        case (Some(left), Some(p), None       ) => layoutForRightOuterChild(t, p, left)
        case (Some(left), Some(p), Some(right)) => layoutForEnclosedChild(t, left, right)
      }
      
      (leftLayout, rightLayout)
      
    } ) getOrElse ("", "")
        
    printWithExistingLayout(traverse, leftLayout.toString, t, rightLayout.toString)
  }
  
  def generate(tree: Tree) = {
        
    val isModified = filter {
      case EmptyTree => false
      case t: Tree => t.pos == NoPosition || t.pos.isRange
    }
    
    def generateSourceCode(self: Transformation[Tree, String], t: Tree): String = t.pos match {
      case NoPosition => 
        prettyPrintTree(self, t)
      case _ =>
        reuseExistingSource(self, t)
    }
    
    recursively(isModified)(generateSourceCode)(tree) 
  }
}