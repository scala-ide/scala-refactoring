package scala.tools.refactoring
package sourcegen

import common.{Change, PimpedTrees, Tracing}

trait SourceGen extends PrettyPrinter with PimpedTrees {
  
  self: LayoutHelper with Tracing =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
 
  import Transformations._
    
  /**
   * For the given Tree, return a pair of Layout elements that contain
   * the leading and trailing original layout elements.
   * 
   * If the tree has no children, then a pair of empty layouts will be
   * returned.
   * */
  def originalEnclosingLayout(t: Tree): (Layout, Layout) = {
    
    val originalTree = findOriginalTree(t).get //exception is ok for now
    
    children(originalTree) match {
      case Nil =>
        NoLayout → NoLayout // XXX? between siblings instead?
      case c => 
        splitLayoutBetweenParentAndFirstChild(parent = originalTree, child = c.head)._1 →
        splitLayoutBetweenLastChildAndParent(child = c.last, parent = originalTree)._2
    }
  }

  def printWithExistingLayout(f: Transformation[Tree, String], left: String, t: Tree, right: String) = {
    
    implicit val currentFile = t.pos.source
    
    def handle(t: Tree) = f(t) getOrElse ""
    
    def handleMany(ts: List[Tree], separator: String = ""): String = ts match {
      case Nil       => ""
      case x :: Nil  => handle(x)
      case x :: rest => handle(x) + separator + handleMany(rest, separator)
    }
    
    def handleModifiers(mods: List[ModifierTree], orig: List[ModifierTree]) = {
      if(mods zip orig exists (x => x._1.pos != x._2.pos)) {
        "mods != modsOrig"
      } else if (mods.isEmpty) { 
         ""
      } else {
        layout(mods.head.pos.start, mods.last.pos.end).toString
      }
    }
    
    val originalTree = findOriginalTree(t).getOrElse{
      val failed = t
      throw new Exception("original tree not found for: "+ t)
    }

    val (l, r) = originalEnclosingLayout(t)
    
    trace("begin:  %s", l.toString)
    trace("end:    %s", r.toString)
    
    val center = (t, originalTree) match {
            
      case (t @ PackageDef(pid, stats), orig: PackageDef) =>
        handle(pid) + handleMany(stats)
      
      case (t @ ClassDef(mods, name, tparams, impl), orig @ ClassDef(ModifiersTree(modsOrig), _, _, _)) =>
        name.toString + handle(impl)
      
      case (t @ TemplateTree(params, earlyBody, parents, self, body), _) =>

        
        handleMany(params, separator = ", ") + handleMany(earlyBody) + handleMany(parents) + handle(self) + handleMany(body)
        
      case (t @ DefDef(ModifiersTree(mods), newName, tparams, vparamss, tpt, rhs), orig @ DefDef(ModifiersTree(modsOrig), name, tparamsOrig, vparamssOrig, tptOrig, rhsOrig)) =>
                
        val nameOrig = NameTree(orig.name, orig.namePosition)
        val modsPrinted = handleModifiers(mods, modsOrig)
        val betweenModsAndName = splitLayoutBetweenSiblings(modsOrig.last, nameOrig)._1
        val nextAfterName = (tparamsOrig ::: vparamssOrig.flatten ::: tptOrig :: rhsOrig :: Nil) filterNot (_ == EmptyTree)
        
        val afterName = nextAfterName.headOption map {
          splitLayoutBetweenSiblings(nameOrig, _)._1
        } getOrElse(splitLayoutBetweenLastChildAndParent(nameOrig, t)._1)
      
        modsPrinted + betweenModsAndName + newName.toString.trim + afterName + 
         handleMany(tparams) + vparamss.map(vparams => handleMany(vparams, ", ")).mkString("") + handle(tpt) + handle(rhs)
        
      case (t @ ValDef(ModifiersTree(mods), newName, tpt, rhs), orig @ ValDef(ModifiersTree(modsOrig), _, tptOrig, rhsOrig)) =>
        
        val nameOrig = NameTree(orig.name, orig.namePosition)
        val modsPrinted = handleModifiers(mods, modsOrig)
        val betweenModsAndName = if(mods.isEmpty) "" else splitLayoutBetweenSiblings(modsOrig.last, nameOrig)._1
        val afterName = splitLayoutBetweenSiblings(nameOrig, if(tpt == EmptyTree) rhsOrig else tptOrig )._1
        
        modsPrinted + betweenModsAndName + newName.toString.trim + afterName + handle(tpt) + handle(rhs)

      case (t: Block, _) => 
        handleMany(t.body)
        
      case (t: TypeTree, _) => 
        t.toString
        
      case (t: TypeDef, _) => 
        t.name
        
      case (t: Ident, _) => 
        if(t.name.toString == "<empty>")
          ""
        else t.name
        
      case (t @ Select(qualifier, selector), orig @ Select(qualifierOrig, _)) =>
        val nameOrig = NameTree(orig.name, orig.namePosition)
        handle(qualifier) + splitLayoutBetweenSiblings(qualifierOrig, nameOrig)._2 + t.symbol.nameString
      
      case (t: Literal, _) =>
        t.toString
        
      case (t @ Apply(fun, args), _) =>
        handle(fun) + handleMany(args, separator = ", ")
        
      case (t @ Import(expr, _), _) =>
        val ts = t.Selectors()
        handle(expr) + handleMany(ts, separator = ", ")
        
      case (t @ ImportSelectorTree(name, rename, _), _) =>
        handle(name) + handle(rename)
        
      case (t: NameTree, _) =>
        t.name.toString
        
      case (t @ SuperConstructorCall(clazz, args), _) =>
        handle(clazz) + handleMany(args, separator = ", ")
        
      case (t, _) => 
        println("printWithExistingLayout: "+ t.getClass.getSimpleName)
        "ø"
      
    }
    
    trace("result:                %s", l.toString + center + r.toString)
    trace("result with enclosing: %s", left + l + center + r + right)
    
    left + l + center + r + right 
  }
  
  def reuseExistingSource(traverse: Transformation[Tree, String], t: Tree): String = context("Reuse "+ t.getClass.getSimpleName) { 
    
    val (leftLayout, rightLayout) = (findOriginalTree(t) map { t =>
    
      val (leftLayout, rightLayout) = (t.originalLeftSibling, t.originalParent, t.originalRightSibling) match {
        case (_,          None,    _          ) => layoutForCuRoot(t)                     \\ (_ => trace("compilation unit root"))
        case (None,       Some(p), None       ) => layoutForSingleChild(t, p)             \\ (_ => trace("single child"))
        case (None,       Some(p), Some(right)) => layoutForLeftOuterChild(t, p, right)   \\ (_ => trace("left outer child"))
        case (Some(left), Some(p), None       ) => layoutForRightOuterChild(t, p, left)   \\ (_ => trace("right outer child"))
        case (Some(left), Some(p), Some(right)) => layoutForEnclosedChild(t, left, right) \\ (_ => trace("enclosed child"))
      }
      
      trace("leading:  %s", leftLayout.toString)
      trace("trailing: %s", rightLayout.toString)
      
      (leftLayout, rightLayout)
      
    } ) getOrElse ("", "")
        
    printWithExistingLayout(traverse, leftLayout.toString, t, rightLayout.toString)
  }
  
  def generate(tree: Tree) = {
        
    val isModified = predicate[Tree] {
      case EmptyTree => false
      case t: Tree => t.pos == NoPosition || t.pos.isRange
    }
    
    def generateSourceCode(self: Transformation[Tree, String], t: Tree): String = t.pos match {
      case NoPosition => 
        prettyPrintTree(self, t)
      case _ =>
        reuseExistingSource(self, t)
    }

    isModified combineRecursively generateSourceCode apply tree
  }
}