/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import PartialFunction.cond
import common.Change
import tools.nsc.symtab.Flags

abstract class EliminateMatch extends MultiStageRefactoring {
  
  val global: tools.nsc.interactive.Global
  
  import global._
  
  type PreparationResult = Transformation[Tree, Tree]
  
  class RefactoringParameters
  
  lazy val HasOptionType = treeHasType("Option")
  
  lazy val HasSomeType = treeHasType("Some")
  
  lazy val HasNoneType = treeHasType("None")
  
  lazy val FalseCase = treeMatches {
    case CaseDef(_, EmptyTree, Literal(Constant(c))) => c == false
  }
  
  lazy val NoneCase = {
    
    val none = newTermName("None")
    
    treeMatches {
      case CaseDef(_, EmptyTree, Select(_, `none`)) => true
    }
  }
  
  lazy val SomeCase = {
        
    val SomeApplication = {
      
      val scala = newTermName("scala")
      val some  = newTermName("Some")
    
      treeMatches {
        case Apply(TypeApply(Select(Select(Ident(`scala`), `some`), _), _), _ :: Nil) => true   
      }
    }
    
    bindingAndBodyFromCaseDefWhereBodyMatches {
      case SomeApplication() | Block(_, SomeApplication()) => true
    }
  }
  
  lazy val OptionCase = bindingAndBodyFromCaseDefWhereBodyMatches {
    case HasOptionType() | HasSomeType() | HasNoneType() => true
  }
  
  lazy val BooleanCase = bindingAndBodyFromCaseDefWhereBodyMatches {
    case body => body.tpe.toString.startsWith("Boolean") 
  }

  
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

          }
      }
    }
    
    def replaceWithCall(fun: String)(mtch: Match, param: Name, body: Tree) = {
      transform {
        case `mtch` =>
          mkCall(mtch.selector, fun, param, body)
      }
    }
    
    def replaceWithExists  = replaceWithCall("exists") _
    def replaceWithFlatMap = replaceWithCall("flatMap") _
    
    s.findSelectedOfType[Match] collect {
      
      /* map */
      
      case mtch @ Match(HasOptionType(), SomeCase(name, body) :: NoneCase() :: Nil) => 
        replaceWithMap(mtch, name, body)
      
      case mtch @ Match(HasOptionType(), NoneCase() :: SomeCase(name, body) :: Nil) => 
        replaceWithMap(mtch, name, body)
      
      /* flatMap */
      
      case mtch @ Match(HasOptionType(), OptionCase(name, body) :: NoneCase() :: Nil) => 
        replaceWithFlatMap(mtch, name, body)
      
      case mtch @ Match(HasOptionType(), NoneCase() :: OptionCase(name, body) :: Nil) => 
        replaceWithFlatMap(mtch, name, body)
      
      /* exists */
        
      case mtch @ Match(_, BooleanCase(name, body) :: FalseCase() :: Nil) => 
        replaceWithExists(mtch, name, body)
      
      case mtch @ Match(_, FalseCase() :: BooleanCase(name, body) :: Nil) => 
        replaceWithExists(mtch, name, body)
        
    } toRight(PreparationError("No elimination candidate found.")) 
  }
    
  def perform(selection: Selection, trans: PreparationResult, name: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    Right(transformFile(selection.file, topdown(matchingChildren(trans))))
  }
  
  
  /*
   * Some helper functions that abstract commonly used extractors:
   */
  
  def treeMatches(pf: PartialFunction[Tree, Boolean]) = new {
    def unapply(t: Tree) = pf.isDefinedAt(t) && pf(t)
  }

  def treeHasType(tpe: String) = new {
    def unapply(t: Tree) = cond(t.tpe) {
      case TypeRef(_, sym, _) => sym.nameString == tpe
    }
  }
  
  def bindingAndBodyFromCaseDefWhereBodyMatches(pf: PartialFunction[Tree, Boolean]) = new {
    
    def unapply(t: Tree): Option[(Name, Tree)] = t match {
      case CaseDef(Apply(_, bind :: _), EmptyTree, body) if pf.isDefinedAt(body) && pf(body) =>

        bind match {
          case Bind(name, _) => Some(Pair(name, body))
          case Ident(name)   => Some(Pair(name, body))
        }
      
      case _ => None
    }
  }
}
