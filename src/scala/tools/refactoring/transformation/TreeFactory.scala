package scala.tools.refactoring.transformation

import scala.tools.refactoring.common.Invisible
import scala.tools.nsc.symtab.Flags

trait TreeFactory {
  
  self: Transform =>
  val global: scala.tools.nsc.Global
  import global._
      
  def mkRenamedSymTree(t: SymTree, name: String) = (t match {
    case i: Ident  => i.copy(name = name)
    case v: ValDef => v.copy(name = name)
    case d: DefDef => d.copy(name = name)
    case b: Bind   => b.copy(name = name)
    case s: Select => s.copy(name = name)
    case t => throw new Exception("Found "+ t.getClass.getName)
  }) setPos t.pos
  
  def mkReturn(s: List[global.Symbol]) = s match {
    case Nil => EmptyTree
    case x :: Nil => Ident(x) setType x.tpe
    case xs => typer.typed(gen.mkTuple(xs map (s => Ident(s) setType s.tpe))) match {
      case t: Apply => t.fun setPos Invisible; t //don't show the TupleX..
      case t => t
    }
  }
  
  def mkCallDefDef(mods: Modifiers = NoMods, name: String, arguments: List[List[Symbol]] = Nil :: Nil, returns: List[Symbol] = Nil): Tree = {
    
     // currying not yet supported
    val args = arguments.head map (s => Ident(s))
    
    val call = Apply(Select(This("") setPos Invisible, name), args)
    
    returns match {
      case Nil => call
      case returns => 
      
        // 'val (a, b) =' is represented by various trees, so we cheat and create the assignment in the name of the value: 
        val valName = returns match {
          case x :: Nil => "val "+ x.name
          case xs => "val ("+ (xs map (_.name) mkString ", ") +")"
        }
                
        ValDef(NoMods, valName, new TypeTree(), call)
    }
  }
  
  def mkDefDef(mods: Modifiers = NoMods, name: String, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree] = Nil) = {
    
    val formalParameters = parameters map ( _ map (s => new ValDef(NoMods, s.nameString, TypeTree(s.tpe), EmptyTree)))
    
    DefDef(mods | Flags.METHOD, name, Nil /*type parameters*/, formalParameters, TypeTree(body.last.tpe), mkBlock(body))
  }
  
  def mkBlock(trees: List[Tree]) = trees match {
    case Nil => throw new Exception("can't make block from 0 trees")
    case x :: Nil => Block(x :: Nil, EmptyTree)
    case xs => Block(xs.init, xs.last)
  }
}
