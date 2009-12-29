package scala.tools.refactoring.transformation

import scala.tools.refactoring.{UnknownPosition, InvisiblePosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait TreeFactory {
  
  self: Transform =>
  val global: scala.tools.nsc.Global
  import global._
  
  def mkReturn(s: List[global.Symbol]) = cleanNoPos(s match {
    case Nil => EmptyTree
    case x :: Nil => Ident(x) setType x.tpe
    case xs => gen.mkTuple(xs map (s => Ident(s) setType s.tpe))
  })
  
  def mkCallDefDef(mods: Modifiers = NoMods, name: String, arguments: List[List[Symbol]] = Nil :: Nil, returns: List[Symbol] = Nil): Tree = cleanNoPos {
    
     // currying not yet supported
    val args = arguments.head map (s => cleanAll(Ident(s)))
    
    returns match {
      case Nil => Apply(Select(This("") setPos InvisiblePosition, name), args)
      
      case returns => 
      
        // 'val (a, b) =' is represented by various trees, so we cheat and create the assignment in the name of the value: 
        val valName = returns match {
          case x :: Nil => "val "+ x.name
          case xs => "val ("+ (xs mkString ", ") +")"
        }
                
        ValDef(NoMods, valName, new TypeTree(), Apply(Select(This("") setPos InvisiblePosition, name), args)) 
    }
  }
  
  def mkDefDef(mods: Modifiers = NoMods, name: String, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree] = Nil) = cleanNoPos {
    
    val formalParameters = parameters map ( _ map (s => cleanAll(ValDef(s, EmptyTree))))
    
    val rhs = body match {
      case Nil => EmptyTree
      case x :: Nil => x
      case xs => Block(xs.init, xs.last)
    }

    DefDef(mods | Flags.METHOD, name, Nil /*type parameters*/, formalParameters, TypeTree(body.last.tpe), rhs)
  }
}
