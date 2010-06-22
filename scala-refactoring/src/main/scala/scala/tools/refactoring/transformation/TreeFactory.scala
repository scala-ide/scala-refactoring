/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import tools.nsc.symtab.Flags

trait TreeFactory {
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  object Invisible extends Position
      
  def mkRenamedSymTree(t: SymTree, name: String): SymTree = (t match {
    case i: Ident    => i.copy(name = name)
    case v: ValDef   => v.copy(name = name)
    case d: DefDef   => d.copy(name = name)
    case b: Bind     => b.copy(name = name)
    case s: Select   => s.copy(name = name)
    case c: ClassDef => c.copy(name = name)
    case s: Super    => s.copy(qual = name)
    case t: This     => t.copy(qual = name)
    case m: ModuleDef=> m.copy(name = name)
    case t: TypeDef  => t.copy(name = name)
    case t: PackageDef => t.copy(pid = Ident(name) setPos t.pid.pos)
    case t => throw new Exception("Found "+ t.getClass.getName)
  }) setPos t.pos
  
  def mkReturn(s: List[Symbol]): Tree = s match {
    case Nil => EmptyTree
    case x :: Nil => Ident(x) setType x.tpe
    case xs => typer.typed(gen.mkTuple(xs map (s => Ident(s) setType s.tpe))) match {
      case t: Apply => t.fun setPos Invisible; t //don't show the TupleX..
      case t => t
    }
  }
  
  def mkValDef(name: String, rhs: Tree): ValDef = rhs match {
    case rhs: Select if rhs.symbol.isMethod =>
      ValDef(NoMods, name, new TypeTree(), Apply(rhs, Ident("_") :: Nil))
    case _ => 
      ValDef(NoMods, name, new TypeTree(), rhs)
  }
  
  def mkCallDefDef(name: String, arguments: List[List[Symbol]] = Nil :: Nil, returns: List[Symbol] = Nil): Tree = {
    
     // currying not yet supported
    val args = arguments.head map (s => Ident(s))
    
    val call = Apply(Select(This("") setPos Invisible, name), args)
    
    returns match {
      case Nil => call
      case returns => 
      
        // 'val (a, b) =' is represented by various trees, so we cheat and create the assignment in the name of the value: 
        val valName = returns match {
          case x :: Nil => x.name.toString
          case xs => "("+ (xs map (_.name) mkString ", ") +")"
        }
                
        ValDef(NoMods, valName, new TypeTree(), call)
    }
  }
  
  def mkDefDef(mods: Modifiers = NoMods, name: String, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree] = Nil): DefDef = {
    
    val formalParameters = parameters map ( _ map (s => new ValDef(Modifiers(Flags.PARAM), s.nameString, TypeTree(s.tpe), EmptyTree)))
    
    DefDef(mods withPosition (Flags.METHOD, NoPosition), name, Nil /*type parameters*/, formalParameters, TypeTree(body.last.tpe), mkBlock(body))
  }
  
  def mkBlock(trees: List[Tree]): Block = trees match {
    case Nil => throw new Exception("can't make block from 0 trees")
    case x :: Nil => Block(x :: Nil, EmptyTree)
    case xs => Block(xs.init, xs.last)
  }
}
