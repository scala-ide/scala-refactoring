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
    lazy val reflect = newTermName("reflect")
    lazy val apply  = newTermName("apply")
    lazy val Nil  = newTermName("Nil")
    lazy val immutable  = newTypeName("immutable")
    lazy val ::  = newTermName("$colon$colon")
    lazy val List  = newTermName("List")
    lazy val Seq = newTermName("Seq")
    lazy val collection = newTermName("collection")
    lazy val immutableTerm  = newTermName("immutable")
    lazy val scalaType = newTypeName("scala")
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
}
