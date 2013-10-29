package scala.tools.refactoring.implementations.modules

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.transformation.TreeTransformations
import scala.tools.refactoring.common.Selections

object InsertionPoints {
  trait InsertionPoint extends RefactoringModule with Selections {
    import global._

    def selection: Selection

    val scopes: List[Tree] =
      selection.collectSelected { t =>
        if (isValidScope(t))
          Some(t)
        else
          None
      }

    def symbolKnownAtInsertionPoint(scope: Tree, s: Symbol): Boolean

    def findScope(scope: Tree): Transformation[Tree, Tree] =
      predicate { t =>
        t.pos == scope.pos
      }

    def insert(inserted: Tree): Transformation[Tree, Tree]

    def insertInScope(scope: Tree, inserted: Tree) =
      topdown {
        matchingChildren {
          findScope(scope) &> insert(inserted)
        }
      }

    def isValidScope(scope: Tree) =
      insert(EmptyTree)(scope).isDefined

    override def preparationError =
      if (scopes.isEmpty)
        Some("No insertion point found")
      else
        super.preparationError
  }

  trait InsertInside extends InsertionPoint {
    import global._

    def beforeInsertionPoint(t: Tree): Boolean

    def symbolKnownAtInsertionPoint(scope: Tree, s: Symbol) = {
      def declInOuterScope = !scope.exists {
        case t: MemberDef => t.symbol == s
        case _ => false
      }

      def symbolIsField = !s.isLocal

      def declBeforeInsertionPoint = scope.children.exists {
        case t: MemberDef if t.symbol == s => beforeInsertionPoint(t)
        case _ => false
      }

      declInOuterScope || symbolIsField || declBeforeInsertionPoint
    }

    def insertInSeq(stats: List[Tree], t: Tree) = {
      val (before, after) = stats.span(beforeInsertionPoint(_))
      before ::: t :: after
    }

    def insertInBlock(original: Tree, t: Tree) =
      if (beforeInsertionPoint(original))
        Block(original, t)
      else
        Block(t, original)

    def insert(inserted: Tree) = {
      transform {
        case t @ Block(stats, expr) =>
          t copy (stats = insertInSeq(stats, inserted)) replaces t
        case t @ Template(_, _, body) =>
          t copy (body = insertInSeq(body, inserted)) replaces t
        case t @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
          t copy (rhs = insertInBlock(rhs, inserted)) replaces t
        case t @ Try(NoBlock(block), _, _) =>
          t copy (block = insertInBlock(block, inserted)) replaces t
      }
    }
  }

  trait AtBeginningOfAnyScope extends InsertInside {
    import global._

    def beforeInsertionPoint(t: Tree) = false
  }

  trait BeforeSelectionInAnyScope extends InsertInside {
    import global._

    def beforeInsertionPoint(t: Tree) =
      !t.pos.isRange || t.pos.end <= selection.pos.start
  }

  trait AfterSelectionInAnyScope extends InsertInside {
    import global._

    def beforeInsertionPoint(t: Tree) =
      !t.pos.isRange || t.pos.start <= selection.pos.end
  }

  trait AfterSelectionInTemplate extends AfterSelectionInAnyScope {
    import global._

    override def isValidScope(t: Tree) = t match {
      case _: Template => true
      case _ => false
    }
  }
}