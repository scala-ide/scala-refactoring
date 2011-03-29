/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import tools.nsc.symtab.Flags
import transformation.TreeExtractors
import transformation.TreeFactory

abstract class EliminateMatch extends MultiStageRefactoring with TreeExtractors with TreeFactory {
  
  val global: tools.nsc.interactive.Global
  
  import global._
  
  type PreparationResult = Transformation[Tree, Tree]
  
  class RefactoringParameters
  
  def prepare(s: Selection) = {
    
    /**
     * When replacing with `map`, we need to remove the explicit `Some` construction
     * in the case body. There are two possible kinds of case bodies: a simple Apply
     * call and a Block that has the Some at its end.
     */
    def replaceWithMap(mtch: Match, param: Name, someCaseBody: Tree) = {
      transform {
        case `mtch` =>
          someCaseBody match {
            
            case Apply(_, arg :: Nil) =>
              mkFunctionCallWithFunctionArgument(mtch.selector, "map", param, arg)
            
            case Block(stmts, Apply(_, arg :: Nil)) =>
              mkFunctionCallWithFunctionArgument(mtch.selector, "map", param, global.Block(stmts, arg))  

            case _ => throw new Exception("Please file a bug report.")
          }
      }
    }
    
    def replaceWithCall(fun: String, mtch: Match, param: Name, body: Tree) = {
      transform {
        case `mtch` =>
          mkFunctionCallWithFunctionArgument(mtch.selector, fun, param, body)
      }
    }
    
    def replaceWithExpr(fun: String, mtch: Match, body: Tree) = {
      transform {
        case `mtch` =>
          mkFunctionCallWithZeroArgFunctionArgument(mtch.selector, fun, body)
      }
    }
    
    def replaceWith(fun: String, mtch: Match) = {
      transform {
        case `mtch` =>
          Select(mtch.selector, newTermName(fun))
      }
    }

    /*
     * A huge thanks to Tony Morris for his scala.Option Cheat Sheet
     */
      
    s.findSelectedOfType[Match] collect {
      
      /* foreach */
        
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body, UnitLit())) =>
        replaceWithCall("foreach", m, name, body)
      
      /* map */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (SomeExpr(_) | Block(_, SomeExpr(_))), NoneExpr())) =>
         replaceWithMap(m, name, body)
        
      /* flatMap */
        
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (HasType("Option" | "Some" | "None")), NoneExpr())) =>
        replaceWithCall("flatMap", m, name, body)
          
      /* isDefined */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(_, Literal(Constant(true)), Literal(Constant(false)))) =>
       replaceWith("isDefined", m)
        
      /* isEmpty */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Literal(Constant(false)), Literal(Constant(true)))) =>
        replaceWith("isEmpty", m)
        
      /* exists */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ HasType("Boolean"), Literal(Constant(false)))) =>
        replaceWithCall("exists", m, name, body)
        
      /* forall */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ HasType("Boolean"), Literal(Constant(true)))) =>
        replaceWithCall("forall", m, name, body)
      
      /* flatten */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Ident(i) , NoneExpr())) if name == i =>
         replaceWith("flatten", m)
      
      /* orElse */
         
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, SomeExpr(Ident(nameInBody)), noneBody @ (HasType("Option" | "Some" | "None")))) if name == nameInBody =>
         replaceWithExpr("orElse", m, noneBody)
      
      /* getOrElse */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, Ident(nameInBody), noneBody)) if name == nameInBody =>
         replaceWithExpr("getOrElse", m, noneBody)
      
      /* toList */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, ListExpr(Ident(name2)), NilExpr())) if name == name2 =>
         replaceWith("toList", m)

    } toRight(PreparationError("No elimination candidate found."))
  }
    
  def perform(selection: Selection, trans: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    Right(transformFile(selection.file, topdown(matchingChildren(trans))))
  }
}
