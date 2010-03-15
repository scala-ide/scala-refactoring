package scala.tools.refactoring.tests.util

import scala.tools.refactoring.util.Selections
import scala.tools.refactoring.transformation.{Transform, TreeFactory}
import scala.tools.refactoring.analysis._
import scala.tools.nsc.util.Position
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.symtab.Flags

trait TestTransform extends Transform with Selections with TreeAnalysis with Indexes with TreeFactory {
  
  val global: scala.tools.nsc.Global
  import global._
  
  def reverseClassParameters(t: Tree) = transform(t) {
    case tpl: Template => tpl copy (body = tpl.body.reverse) setPos tpl.pos
  }
  
  def insertNewMethod(t: Tree) = transform(t) {
    case tpl @ Template(parents, self, body) => {
      
      val typ: TypeTree = body(1) match {
        case tree: DefDef => tree.tpt.asInstanceOf[TypeTree]
      }
      
      val v = ValDef(NoMods, newTermName("sample"), TypeTree(typ.tpe), EmptyTree)

      val block = Block( Literal(555) :: v :: Nil, EmptyTree)
                
      val d = DefDef(Modifiers(Flags.METHOD), newTermName("method"), Nil, Nil, TypeTree(typ.tpe), block)
      
      new Template(parents, self, d :: body) setPos tpl.pos
    }
  }
  
  def copyLastMethod(t: Tree) = transform(t) {
    case tpl @ Template(parents, self, body) => Template(parents, self, body.last :: body) setPos tpl.pos
  }
  
  def newMethodFromExistingBody(t: Tree) = transform(t) {
    case tpl @ Template(parents, self, body) => {
          
      val typ: TypeTree = body(1) match {
        case tree: DefDef => tree.tpt.asInstanceOf[TypeTree]
      }
      
      val v = ValDef(NoMods, "arg", TypeTree(typ.tpe), EmptyTree)
      
      val rhs = body.last match {
        case tree: ValOrDefDef => tree.rhs
      }

      val d = DefDef(Modifiers(Flags.METHOD), "newMethod", Nil, (v :: Nil) :: Nil, TypeTree(typ.tpe), rhs)
      
      new Template(parents, self, d :: body) setPos tpl.pos
    }
  }
  
  def bodyInBody(t: Tree) = transform(t) {
    case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs: Block) if defdef.pos.isRange => {
        
      val newDef = DefDef(Modifiers(Flags.METHOD), "innerMethod", Nil, Nil, TypeTree(rhs.expr.tpe), rhs) 
    
      val newRhs = Block(
            newDef
            :: Nil
            , Apply(Select(This(""), "innerMethod"), Nil))
      
      
      new DefDef(mods, name, tparams, vparamss, tpt, newRhs)  setPos defdef.pos
    }
  }  
  
  def newMethod(t: Tree) = transform(t) {
    case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs: Block) if defdef.pos.isRange => {

      index.processTree(t)
      
      val selection = new TreeSelection(rhs.stats(1))
      
      val selected = selection.trees.head
      val parameters = inboundLocalDependencies(selection, defdef.symbol)
      
      val call    = mkCallDefDef(NoMods, "innerMethod", parameters :: Nil, outboundLocalDependencies(selection, defdef.symbol))
      
      val returns = mkReturn(outboundLocalDependencies(selection, defdef.symbol))
      val newDef  = mkDefDef(NoMods, "innerMethod", parameters :: Nil, selected :: returns :: Nil)
      
      val newRhs = Block(
            newDef :: rhs.stats.head :: call :: Nil
            , rhs.expr)
      
      new DefDef(mods, name, tparams, vparamss, tpt, newRhs) setPos defdef.pos
    }
  }
}
