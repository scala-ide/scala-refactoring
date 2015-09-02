/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import PartialFunction.cond

trait TreeExtractors {

  this: common.CompilerAccess =>

  import global._

  object Names {
    lazy val scala = newTermName("scala")
    lazy val pkg = newTermName("package")
    lazy val None  = newTermName("None")
    lazy val Some  = newTermName("Some")
    lazy val Predef  = newTermName("Predef")
    lazy val apply  = newTermName("apply")
    lazy val Nil  = newTermName("Nil")
    lazy val immutable  = newTypeName("immutable")
    lazy val ::  = newTermName("$colon$colon")
    lazy val List  = newTermName("List")
  }

  /**
   * An extractor for the Some constructor.
   */
  object SomeExpr {
    def unapply(t: Tree): Option[Tree] = t match {
      case Apply(TypeApply(Select(Select(Ident(Names.scala), Names.Some), _), (_: TypeTree) :: Nil), argToSome :: Nil) => Some(argToSome)
      case _ => None
    }
  }

  /**
   * A boolean extractor for the None constructor.
   */
  object NoneExpr {
    def unapply(t: Tree) = cond(t) {
      case Select(Ident(Names.scala), Names.None) => true
    }
  }

  /**
   * An extractor for the List constructor `List` or ::
   */
  object ListExpr {
    def unapply(t: Tree): Option[Tree] = t match {
      case Block(
            (ValDef(_, v1, _, arg)) :: Nil, Apply(TypeApply(Select(Select(This(Names.immutable), Names.Nil), Names.::), (_: TypeTree) :: Nil),
            Ident(v2) :: Nil)) if v1 == v2 =>
        Some(arg)
      case Apply(TypeApply(Select(Select(This(Names.immutable), Names.List), Names.apply), (_: TypeTree) :: Nil), arg :: Nil) =>
        Some(arg)
      case _ =>
        None
    }
  }

  /**
   * A boolean extractor for the Nil object.
   */
  object NilExpr {
    def unapply(t: Tree) = cond(t) {
      case Select(This(Names.immutable), Names.Nil) => true
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

    def unapply(t: Tree) = {
      getTypeName(t.tpe)
    }
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
        case CaseDef(Select(Ident(Names.scala), Names.None), EmptyTree, body) => Some(body)
        case CaseDef(_, EmptyTree, body @ NoneExpr()) => Some(body)
        case _ => None
      }
    }

    object SomeCaseDef {
      def unapply(t: Tree): Option[(TermName, Tree)] = t match {
        case CaseDef(Apply(tt: TypeTree, bind :: _), EmptyTree, body) =>

          tt.original match {
            case Select(Ident(Names.scala), Names.Some) =>
              bind match {
                case Bind(name: TermName, Ident(nme.WILDCARD)) => Some(Pair(name, body))
                case Ident(name: TermName)   => Some(Pair(name, body))
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    }

    def unapply(ts: List[Tree]): Option[Triple[TermName, Tree, Tree]] = {

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
