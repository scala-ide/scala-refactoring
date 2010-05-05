package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile
import tools.nsc.util.RangePosition
import tools.nsc.symtab.{Flags, Names, Symbols}
import reflect.ClassManifest.fromClass

/**
 * A bunch of implicit conversions for ASTs and other helper
 * functions that work on trees. Users of the trait need to
 * provide the means to access a file's corresponding tree.
 * 
 * */
trait PimpedTrees {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  /**
   * Returns the tree that is contained in this file.
   * Typically done with global.unitOfFile.
   * */
  def treeForFile(file: AbstractFile): Option[Tree]
    
  def cuRoot(p: global.Position) = treeForFile(p.source.file)
  
  /**
   * Extract the modifiers with their position from a Modifiers
   * object and returns them in the order they appear in the
   * source code.
   * */
  object ModifiersTree {
    def unapply(m: global.Modifiers) = {
      Some(m.positions.toList map {m => 
        ModifierTree(m._1, m._2 withEnd (m._2.end + 1))
      } sortBy (_.pos.start))
    }
  }
  
  implicit def importTreeSelectorsExtractor(t: Import) = new {
    // work around for https://lampsvn.epfl.ch/trac/scala/ticket/3392
    def Selectors(ss: List[ImportSelector] = t.selectors) = ss map { imp: ImportSelector =>
    
      val name = NameTree(imp.name, new RangePosition(t.pos.source, imp.namePos,   imp.namePos,   imp.namePos   + imp.name.length))
      
      if(imp.renamePos < 0 || imp.name == imp.rename) {
        ImportSelectorTree(
          name, 
          EmptyTree,
          name.pos)
      } else {
        val rename = NameTree(imp.rename, new RangePosition(t.pos.source, imp.renamePos, imp.renamePos, imp.renamePos + imp.rename.length)) 
        ImportSelectorTree(
          name, 
          rename,
          name.pos withPoint rename.pos.start withEnd rename.pos.end)
      }
    }
    
    object Selectors {
      def unapply(ss: List[ImportSelector]) = {
        Some(Selectors(ss))
      }
    }
  }
  
  implicit def nameTreeExtractor(t: Tree) = new {
    object Name {
      def unapply(name: Name) = {
        Some(NameTree(name, pimpPositions(t).namePosition))
      }
    }
  }
  
  /**
   * Represent a name as a tree, including its position.
   * */
  case class NameTree(name: Name, override val pos: Position) extends Tree {
    setPos(pos)
  }
  
  /**
   * Represent a modifier as a tree, including its position.
   * */
  case class ModifierTree(flag: Long, override val pos: Position) extends Tree {
    setPos(pos)
  }
  
  /**
   * Represent an import as a tree, including both names as trees.
   * */
  case class ImportSelectorTree(name: NameTree, rename: Tree, override val pos: Position) extends Tree {
    setPos(pos)
  }
  
  case class SuperConstructorCall(clazz: Tree, args: List[Tree]) extends Tree {
    setPos(clazz.pos withEnd args.lastOption.getOrElse(clazz).pos.end)
  }
  
  object TemplateTree {
    def unapply(t: Tree) = t match {
      case tpl: Template => 
      
        def empty(t: Tree) = t == EmptyTree || t == emptyValDef
            
        val (classParams, restBody) = tpl.body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) || mods.hasFlag(Flags.PARAM) 
          case _ => false
        }
        
        val(earlyBody, _) = restBody.filter(_.pos.isRange).partition((t: Tree) => tpl.parents.exists(t.pos precedes _.pos))
                
        val body = (restBody filterNot (earlyBody contains)).filter(t => t.pos.isRange || t.pos == NoPosition).filterNot(empty)
        
        val superCalls = tpl.parents filterNot empty map { superClass =>
        
         val superArgs = earlyBody collect {
           case DefDef(_, _, _, _, _, Block(Apply(_, args) :: _, _)) if args.exists(superClass.pos precedes _.pos) => args
         } flatten
        
          SuperConstructorCall(superClass, superArgs)
        } 

        Some((classParams, Nil: List[Tree] /*early body*/, superCalls, if(empty(tpl.self)) EmptyTree else tpl.self, body))
      
      case _ => 
        None
    }
  }
  
  /**
   * Add some methods to Tree that make it easier to compare
   * Trees by position and to extract the position of a tree's
   * name, which is tricky for Selects.
   * */
  implicit def pimpPositions(t: Tree) = new {
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && t.pos.source == p.source
    def samePos(o: Tree)    : Boolean = samePos(o.pos)
    def sameTree(o: Tree)   : Boolean = samePos(o.pos) && fromClass(o.getClass).equals(fromClass(t.getClass))
    def namePosition: Position = t match {
      case t: ClassDef    => t.pos withStart (t.pos.point) withEnd (t.pos.point + t.name.toString.trim.length)
      case t: ValOrDefDef => t.pos withStart (t.pos.point) withEnd (t.pos.point + t.name.toString.trim.length)
      case t @ Select(qualifier, selector) => 
      
        if (qualifier.pos.isRange && qualifier.pos.start > t.pos.start) /* e.g. !true */ {
          t.pos withEnd qualifier.pos.start
        } else if (qualifier.pos.isRange && t.symbol != NoSymbol) {
          t.pos withStart (t.pos.end - t.symbol.nameString.length)
        } else if (qualifier.pos.isRange) {
          t.pos withStart (t.pos.point.max(qualifier.pos.end + 1))
        } else {
          throw new Exception("Unreachable")
        }
        
      case _ => throw new Exception("uhoh")
    }
  }
  
  /**
   * Unify the children of a Block tree and sort them 
   * in the same order they appear in the source code.
   * */
  implicit def blockToBlockWithOrderedChildren(t: Block) = new {
    def body: List[Tree] = if(t.expr.pos.isRange && (t.expr.pos precedes t.stats.head.pos))
      t.expr :: t.stats
    else
      t.stats ::: t.expr :: Nil
  }
  
  /**
   * Make a Tree aware of its parent and siblings. Note
   * that these are expensive operations because they
   * traverse the whole compilation unit.
   * */
  implicit def treeToPimpedTree(t: Tree) = new {
    def childTrees: List[Tree] = children(t)
    def originalParent = cuRoot(t.pos) flatMap { root =>
    
      def find(root: Tree): Option[Tree] = {
        val cs = children(root)
        
        if(cs.exists(_ sameTree t))
          Some(root)
        else
          cs.flatMap(find).lastOption
      }
      find(root)
    }
    def originalLeftSibling  = findSibling(originalParent, 1, 0)
    def originalRightSibling = findSibling(originalParent, 0, 1)
    private def findSibling(parent: Option[Tree], compareIndex: Int, returnIndex: Int) = parent flatMap 
      (children(_) filter (_.pos.isRange) sliding 2 find (_ lift compareIndex map (_ samePos t) getOrElse false) flatMap (_ lift returnIndex))
  }
  
  /**
   * Given a Position, returns the tree in that compilation
   * unit that inhabits that position.
   * */
  def findOriginalTreeFromPosition(p: Position): Option[List[Tree]] = {
    def find(t: Tree): List[Tree] = {
      (if(t samePos p)
        t :: Nil
      else 
        Nil) ::: children(t).map(find).flatten
    }
    
    cuRoot(p) map find
  }

  /**
   * Find a tree by its position and make sure that the trees
   * or of the same type. This is necessary because some trees
   * have the same position, for example, a compilation unit
   * without an explicit package and just a single top level
   * class, then the package and the class will have the same
   * position.
   * 
   * If multiple trees are candidates, then take the last one, 
   * because it is likely more specific.
   * */
  def findOriginalTree(t: Tree) = findOriginalTreeFromPosition(t.pos) flatMap (_ filter (_ sameTree t ) lastOption)
  
  /**
   * Returns all children that have a representation in the source code.
   * This includes Name and Modifier trees and excludes everything that
   * has no Position or is an EmptyTree.
   * */
  def children(t: Tree) = (t match {
    
    case t @ PackageDef(pid, stats) => 
      pid :: stats
    
    case t @ ClassDef(ModifiersTree(mods), name, tparams, impl) =>
      mods ::: NameTree(name, t.namePosition) :: tparams ::: impl :: Nil
      
    case t @ TemplateTree(params, earlyBody, parents, self, body) =>
      params ::: earlyBody ::: parents ::: self :: body

    case t @ ValDef(ModifiersTree(mods), name, tpt, rhs) =>
      mods ::: NameTree(name, t.namePosition) :: tpt :: rhs :: Nil
     
    case t @ DefDef(ModifiersTree(mods), name, tparams, vparamss, tpt, rhs) =>
      mods ::: NameTree(name, t.namePosition) :: tparams ::: vparamss.flatten ::: tpt :: rhs :: Nil
     
    case _: TypeTree | _: TypeDef | _: Literal | _: Ident | _: ModifierTree | _: NameTree => Nil
    
    case t @ Apply(fun, args) =>
      fun :: args
      
    case t @ Select(qualifier, selector) =>
      qualifier :: NameTree(selector, t.namePosition) :: Nil
      
    case t: Block =>
      t.body
      
    case t @ Import(expr, _) =>
      expr :: t.Selectors()
      
    case ImportSelectorTree(name, rename, _) =>
      name :: rename :: Nil
      
    case SuperConstructorCall(clazz, args) =>
      clazz :: args
    
    case _ => throw new Exception("Unhandled tree: "+ t.getClass.getSimpleName)
     
  }) filterNot (_ == EmptyTree) filterNot (_.pos == NoPosition)
  
}