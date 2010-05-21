package scala.tools.refactoring.common

import tools.nsc.symtab.Flags
import tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens

/**
 * A set of custom Tree subclasses to make working with
 * the AST easier.
 * 
 * */
trait CustomTrees {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  def keepTree(t: Tree) = !t.isEmpty && (t.pos.isRange || t.pos == NoPosition)
  
  /**
   * Represent a Name as a tree, including its position.
   * */
  case class NameTree(name: global.Name) extends global.Tree {
    if (name.toString == "<none>") Predef.error("Name cannot be <none>, NoSymbol used?")
    def nameString = name.toString.trim
  }
  
  /**
   * Represent a modifier as a tree, including its position.
   * */
  case class ModifierTree(flag: Long) extends global.Tree {
    
    import Flags._
    
    def nameString = flag match {
      case 0            => ""
      case TRAIT        => "trait"
      case METHOD       => "def"
      case FINAL        => "final"
      case IMPLICIT     => "implicit"
      case PRIVATE      => "private"
      case PROTECTED    => "protected"
      case SEALED       => "sealed"
      case OVERRIDE     => "override"
      case CASE         => "case"
      case ABSTRACT     => "abstract"
      case PARAM        => ""
      case LAZY         => "lazy"
      case Tokens.VAL   => "val"
      case Tokens.VAR   => "var"
      case Tokens.TYPE  => "type"
      case Tokens.DEF   => "def"
      case _            => "<unknown>: " + flagsToString(flag)
    }
  } 
    
  /**
   * Extract the modifiers with their position from a Modifiers
   * object.
   * */
  object ModifierTree {
    def unapply(m: global.Modifiers) = {
      Some(m.positions.toList map {
        case (flag, global.NoPosition) => 
          ModifierTree(flag)
        case (flag, pos) =>
          ModifierTree(flag) setPos (pos withEnd (pos.end + 1))
      })
    }
  }
  
  /**
   * Represent an import selector as a tree, including both names as trees.
   * */
  case class ImportSelectorTree(name: NameTree, rename: global.Tree) extends global.Tree
  
  /**
   * Import selectors are not trees, but we can provide an extractor
   * that converts the ImportSelectors into our own ImportSelectorTrees.
   * */
  implicit def importToImportSelectorTreeExtractor(t: global.Import) = new {
    // work around for https://lampsvn.epfl.ch/trac/scala/ticket/3392
    def Selectors(ss: List[global.ImportSelector] = t.selectors) = ss map { imp: global.ImportSelector =>
    
      val name = NameTree(imp.name) setPos new RangePosition(t.pos.source, imp.namePos, imp.namePos, imp.namePos + imp.name.length)
      
      if(imp.renamePos < 0 || imp.name == imp.rename) {
        ImportSelectorTree(
          name, 
          global.EmptyTree) setPos name.pos
      } else {
        val rename = NameTree(imp.rename) setPos new RangePosition(t.pos.source, imp.renamePos, imp.renamePos, imp.renamePos + imp.rename.length) 
        ImportSelectorTree(
          name, 
          rename) setPos (name.pos withPoint rename.pos.start withEnd rename.pos.end)
      }
    }
    
    object Selectors {
      def unapply(ss: List[global.ImportSelector]) = {
        Some(Selectors(ss))
      }
    }
  }
  
  /**
   * The call to the super constructor in a class:
   * class A(i: Int) extends B(i)
   *                         ^^^^ 
   * */
  case class SuperConstructorCall(clazz: global.Tree, args: List[global.Tree]) extends global.Tree {
    if(clazz.pos != global.NoPosition) setPos(clazz.pos withEnd args.lastOption.getOrElse(clazz).pos.end)
  }
  
  /**
   * Representation of self type annotations:
   *   self: A with B =>
   *   ^^^^^^^^^^^^^^
   * */
  case class SelfTypeTree(name: NameTree, types: List[global.Tree], orig: Tree) extends global.Tree
    
  /**
   * Unify the children of a Block tree and sort them 
   * in the same order they appear in the source code.
   * */
  object BlockExtractor {
    def unapply(t: Block) = Some((if(t.expr.pos.isRange && t.stats.size > 0 && (t.expr.pos precedes t.stats.head.pos))
      t.expr :: t.stats
    else
      t.stats ::: t.expr :: Nil) filter keepTree) 
  }
}