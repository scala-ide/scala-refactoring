/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import tools.nsc.symtab.Flags
import transformation.TreeExtractors

abstract class EliminateMatch extends MultiStageRefactoring with TreeExtractors {
  
  val global: tools.nsc.interactive.Global
  
  import global._
  
  type PreparationResult = Transformation[Tree, Tree]
  
  class RefactoringParameters
  
  def prepare(s: Selection) = {
    
    /**
     * Creates a function call `replacementFunction` on the selector and passes a function with
     * a single parameter `param` and the body `body`.
     */
    def mkCall(selector: Tree, replacementFunction: String, param: Name, body: Tree) = {
      Apply(
        Select(
          selector,
          newTermName(replacementFunction)), 
        List(Function(List(ValDef(Modifiers(Flags.PARAM), param, EmptyTree, EmptyTree)), body))
      ) typeFrom body
    }
    
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
              mkCall(mtch.selector, "map", param, arg)
            
            case Block(stmts, Apply(_, arg :: Nil)) =>
              mkCall(mtch.selector, "map", param, global.Block(stmts, arg))  

            case _ => throw new Exception("Please file a bug report.")
          }
      }
    }
    
    def replaceWithCall(fun: String, mtch: Match, param: Name, body: Tree) = {
      transform {
        case `mtch` =>
          mkCall(mtch.selector, fun, param, body)
      }
    }
    
    def replaceWithIsDefined(mtch: Match) = {
      transform {
          case `mtch` =>
            Select(mtch.selector, newTermName("isDefined"))            
        }     
    }
      
    s.findSelectedOfType[Match] collect {
      
      /* foreach */
        
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body, UnitLit())) =>
        replaceWithCall("foreach", m, name, body)
      
      /* map */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (SomeExpr() | Block(_, SomeExpr())), NoneExpr())) => 
         replaceWithMap(m, name, body)
        
      /* flatMap */
        
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ (HasType("Option" | "Some" | "None")), NoneExpr())) => 
        replaceWithCall("flatMap", m, name, body)
          
      /* isDefined */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(_, Literal(Constant(true)), Literal(Constant(false)))) =>
       replaceWithIsDefined(m)
        
      /* exists */
      
      case m @ Match(HasType("Option"), MatchOnSomeAndNone(name, body @ HasType("Boolean"), Literal(Constant(false)))) =>         
        replaceWithCall("exists", m, name, body)

    } toRight(PreparationError("No elimination candidate found.")) 
  }
    
  def perform(selection: Selection, trans: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    Right(transformFile(selection.file, topdown(matchingChildren(trans))))
  }
}
