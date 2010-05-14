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
trait PimpedTrees extends AdditionalTreeMethods with CustomTrees {
  
  val global: scala.tools.nsc.interactive.Global
  import global._

  /**
   * Returns the tree that is contained in this file.
   * Typically done with global.unitOfFile.
   * */
  def treeForFile(file: AbstractFile): Option[Tree]
    
  /**
   * Returns the compilation unit root for that position.
   * */  
  def cuRoot(p: Position): Option[Tree] = if (p == NoPosition) None else treeForFile(p.source.file)

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
  def findOriginalTree(t: Tree): Option[Tree] = findOriginalTreeFromPosition(t.pos) flatMap (_ filter (_ sameTree t ) lastOption)
  
  /**
   * Returns all children that have a representation in the source code.
   * This includes Name and Modifier trees and excludes everything that
   * has no Position or is an EmptyTree.
   * */
  def children(t: Tree): List[Tree] = (t match {
    
    case t @ PackageDef(pid, stats) => 
      pid :: stats
    
    case t @ ClassDef(ModifierTree(mods), name, tparams, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: impl :: Nil
      
    case t @ ModuleDef(ModifierTree(mods), name, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: impl :: Nil
      
    case t @ TemplateExtractor(params, earlyBody, parents, self, body) =>
      params ::: earlyBody ::: parents ::: self :: body

    case t @ ValDef(ModifierTree(mods), name, tpt, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tpt :: rhs :: Nil
     
    case t @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: vparamss.flatten ::: tpt :: rhs :: Nil
     
    case t: TypeTree =>
      if(t.original != null) t.original :: Nil else Nil
      
    case AppliedTypeTree(tpt, args) =>
      tpt :: args
      
    case TypeDef(ModifierTree(mods), name, tparams, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: rhs :: Nil
    
    case _: Literal | _: Ident | _: ModifierTree | _: NameTree | _: This => Nil
    
    case t @ Apply(fun, args) =>
      fun :: args
      
    case t @ Select(qualifier: This, selector) if qualifier.pos == NoPosition && t.pos.start == t.pos.point =>
      (NameTree(selector) setPos t.namePosition) :: Nil
      
    case t @ Select(qualifier, selector) =>
      qualifier :: (NameTree(selector) setPos t.namePosition) :: Nil
      
    case BlockExtractor(stats) =>
      stats
      
    case Return(expr) =>
      expr :: Nil
      
    case New(tpt) =>
      tpt :: Nil
      
    case t @ Import(expr, _) =>
      expr :: t.Selectors()
      
    case ImportSelectorTree(name, rename) =>
      name :: rename :: Nil
      
    case SuperConstructorCall(clazz, args) =>
      clazz :: args
      
    case SelfTypeTree(name, types) =>
      name :: types
      
    case TypeApply(fun, args) =>
      fun :: args
      
    case Function(vparams, body) =>
      vparams ::: body :: Nil
      
    case If(cond, thenp, elsep) =>
      cond :: thenp :: elsep :: Nil
      
    case TypeBoundsTree(lo, hi) =>
      lo :: hi :: Nil
    
    case _ => throw new Exception("Unhandled tree: "+ t.getClass.getSimpleName)
     
  }) filterNot (_.isEmpty)
  
}