package scala.tools.refactor.transform

import scala.tools.refactor.Compiler
import scala.tools.refactor.UnknownPosition
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait Transform {
  
  self: scala.tools.refactor.Compiler =>
  import compiler._
  
  private object PositionSetter extends Traverser {
     override def traverse(tree: Tree): Unit = {
       
       if(tree.pos == NoPosition) {
         tree setPos UnknownPosition
       }

       super.traverse(tree)
     }
  }
  
  def cleanTree[T <: Tree](body: => T): T = {
    val tree: T = body
    PositionSetter.traverse(tree)
    tree
  }
  
  def reverseClassParameters = new Transformer {
    override def transform(tree: Tree): Tree = super.transform(tree) match {
      case Template(parents, self, body) => new Template(parents, self, body.reverse).copyAttrs(tree)
      case x => x
    }
  }
  
  def insertValue = new Transformer {
    override def transform(tree: Tree): Tree = {
      val tree1 = super.transform(tree) 
      tree1 match {
        
        case tpl @ Template(parents, self, body) => 
          
          val typ: TypeTree = body(1) match {
            case tree: DefDef => tree.tpt.asInstanceOf[TypeTree]
          }
          
          val v = ValDef(NoMods, newTermName("sample"), TypeTree(typ.tpe), Literal(5))
          
          val rhs = body.last match {
            case tree: ValOrDefDef => tree.rhs
          }

          val block = Block( Literal(555) :: v :: Nil, EmptyTree)
          
          //val d =  DefDef(Modifiers(0) withPosition (Tokens.DEF, UnknownPosition), newTermName("newDefDef"), Nil, (v :: Nil) :: Nil, TypeTree(typ.tpe) setPos UnknownPosition, rhs) setPos UnknownPosition
          
          val d = cleanTree {
            DefDef(Modifiers(Flags.METHOD), newTermName("method"), Nil, Nil, TypeTree(typ.tpe), block)
          }
          
          new Template(parents, self, d :: body).copyAttrs(tree)
        
        case x => x
      }
    }
  }
}
