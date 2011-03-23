/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import PartialFunction.cond

trait TreeExtractors {
  
  this: common.CompilerAccess =>
  
  import global._

  lazy val scala = newTermName("scala")
  lazy val none  = newTermName("None")
  lazy val some  = newTermName("Some")
  
  /**
   * A boolean extractor for the Some constructor.
   */
  object SomeExpr {
    def unapply(t: Tree) = cond(t) {
      case Apply(TypeApply(Select(Select(Ident(`scala`), `some`), _), _), _ :: Nil) => true
    }
  }
  
  /**
   * A boolean extractor for the None constructor.
   */
  object NoneExpr {
    def unapply(t: Tree) = cond(t) {
      case Select(Ident(`scala`), `none`) => true
    }
  }

  /**
   * An extractor for the () literal tree.
   */
  object UnitLit {
    def unapply(t: Tree) = cond(t) {
      case Literal(c) => c.tag == UnitTag
    }
  }
  
  /**
   * An extractor that returns the name of a tree's
   * type as a String.
   */
  object HasType {
    
    def getTypeName(t: Type): Option[String] = t match {
      case TypeRef(_, sym, _) => 
        Some(sym.nameString)
      case ConstantType(value) =>  
        getTypeName(value.tpe)
      case _ => None
    }
    
    def unapply(t: Tree) = getTypeName(t.tpe)
  }
  
  /**
   * True if the tree's type is Unit
   */
  def hasUnitType(t: Tree) = t.tpe match {
    case TypeRef(_, sym, _) => sym == definitions.UnitClass
    case _ => false
  }
  
  /**
   * An extractor that matches on a Some/None pattern match.
   * 
   * The match is only successful if there's exactly one Some
   * and one None case (or a default case that results in `None`),
   * with no guards.
   * 
   * The result is a triple with the name bound that was bound
   * to the content of Some and the bodies of both cases.
   */
  object MatchOnSomeAndNone {
    
    object NoneCaseDef {
                
      def unapply(t: Tree): Option[Tree] = t match {
        case CaseDef(Select(Ident(`scala`), `none`), EmptyTree, body) => Some(body)
        case CaseDef(_, EmptyTree, body @ NoneExpr()) => Some(body)
        case _ => None
      }
    }
    
    object SomeCaseDef {
      def unapply(t: Tree): Option[(Name, Tree)] = t match {
        case CaseDef(Apply(tt: TypeTree, bind :: _), EmptyTree, body) =>
    
          tt.original match {
            case Select(Ident(`scala`), `some`) =>           
              bind match {
                case Bind(name, _) => Some(Pair(name, body))
                case Ident(name)   => Some(Pair(name, body))
              }
            case _ => None
          }
        case _ => None    
      }
    }
    
    def unapply(ts: List[Tree]): Option[Triple[Name, Tree, Tree]] = {
      
      ts match {
        case  SomeCaseDef(name, someBody) :: NoneCaseDef(noneBody) :: Nil =>
          Some(Triple(name, someBody, noneBody))
          
        case NoneCaseDef(noneBody) :: SomeCaseDef(name, someBody) :: Nil =>
          Some(Triple(name, someBody, noneBody))
        
        case _ => 
          None
      }
    }
  }
}