/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import tools.nsc.symtab.Flags

abstract class EliminateMatch extends MultiStageRefactoring {
  
  val global: tools.nsc.interactive.Global
  
  import global._
  
  type PreparationResult = (Match, Name, Tree)
  
  class RefactoringParameters
  
  object HasOptionType {
    def unapply(t: Tree): Boolean = t.tpe match {
      case TypeRef(_, sym, _) if sym.nameString == "Option" => true
      case _ => false  
    }
  }
  
  object SomeCase {
    
    object HasSomeType {
      def unapply(t: Tree): Option[Tree] = {
        val some = newTermName("Some")
        
        t match {
          case t: TypeTree => t.original match {
            case Select(_, `some`) => Some(t)
            case _ => None
          }
          case _ => t.tpe match {
            case TypeRef(_, sym, _) if sym.nameString == "Some" => Some(t)
            case _ => None  
          }
        }
      }
    }
    
    def unapply(t: Tree): Option[(Name, Tree)] = t match {
      case CaseDef(Apply(HasSomeType(_), bind :: _), EmptyTree, HasSomeType(body)) =>
        bind match {
          case Bind(name, _) => Some(Pair(name, body))
          case Ident(name)   => Some(Pair(name, body))
        }
      case _ => 
        None
    }
  }
  
  object NoneCase {
    
    val none = newTermName("None")
    
    def unapply(t: Tree): Boolean = t match {
      case CaseDef(_, EmptyTree, Select(_, `none`)) =>
        true
      case _ => 
        false
    }
  }
  
  def prepare(s: Selection) = {
    
    s.findSelectedOfType[Match] collect {
      case mtch @ Match(HasOptionType(), SomeCase(name, body) :: NoneCase() :: Nil) => 
        (mtch, name, body)
      case mtch @ Match(HasOptionType(), NoneCase() :: SomeCase(name, body) :: Nil) => 
        (mtch, name, body)
    } toRight(PreparationError("no elimination candidate found")) 
  }
    
  def perform(selection: Selection, optionMatch: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    
    val (mtch, name, body) = optionMatch
    
    def mkCallToMap(fun: Tree) = {
      Apply(
        Select(
          mtch.selector, 
          newTermName("map")), 
        Function(ValDef(Modifiers(Flags.PARAM), name, EmptyTree, EmptyTree) :: Nil, fun) :: Nil) typeFrom fun
    }
    
    val eliminatMatch = transform {
     
      case `mtch` =>
        body match {
          
          case Apply(_, arg :: Nil) =>
            mkCallToMap(arg)
          
          case Block(stmts, Apply(_, arg :: Nil)) =>
            mkCallToMap(global.Block(stmts, arg))  
          
          case _ => return Left(RefactoringError("Unable to eliminate match."))
        }
    }
    
    Right(transformFile(selection.file, topdown(matchingChildren(eliminatMatch))))
  }
}
