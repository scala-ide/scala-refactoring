package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.transformation.TreeTransformations

trait InsertionPoints extends Selections with TreeTransformations { self: CompilerAccess =>
  import global._

  /**
   * An insertion position is a function that may be defined for
   * an enclosing tree. When defined, it returns an instance of
   * an InsertionPoint.
   */
  type InsertionPosition = PartialFunction[Tree, InsertionPoint]

  /**
   * A concrete position for tree insertions.
   */
  case class InsertionPoint(enclosing: Tree, mkEnclosing: Tree => Tree) extends (Tree => Tree) {
    /**
     * Returns a new tree that contains every tree of enclosing
     * and insertion inserted at the appropriate position.
     */
    def apply(insertion: Tree) = mkEnclosing(insertion)
  }

  private def insertInSeq(stats: List[Tree], insertion: Tree, isBeforeInsertionPoint: Position => Boolean) = {
    val (before, after) = stats.span((t: Tree) => isBeforeInsertionPoint(t.pos))
    before ::: insertion :: after ::: Nil
  }

  /**
   * Inserts trees as the first statement in a method body.
   */
  lazy val atBeginningOfDefDef: InsertionPosition = {
    case enclosing @ DefDef(_, _, _, _, _, Block(stats, expr)) =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (rhs = mkBlock(insertInSeq(stats :+ expr, insertion, _ => false)))
      })
    case enclosing @ DefDef(_, _, _, _, _, rhs) =>
      InsertionPoint(enclosing, { insertion: Tree =>
        enclosing copy (rhs = mkBlock(insertion :: rhs :: Nil))
      })
  }

  /**
   * Inserts trees as the first statement in a function body.
   */
  lazy val atBeginningOfFunction: InsertionPosition = {
    case enclosing @ Function(_, Block(stats, expr)) =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (body = mkBlock(insertInSeq(stats :+ expr, insertion, _ => false)))
      })
    case enclosing @ Function(_, body) =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (body = mkBlock(insertion :: body :: Nil))
      })
  }

  /**
   * Inserts trees as additional method parameters.
   */
  lazy val atEndOfParameterList: InsertionPosition = ???

  implicit class SelectionDependentInsertionPoints(selection: Selection) {
    private def isBeforeSelectionIn(enclosing: Tree)(pos: Position) = {
      val startOfTopLevelTree = enclosing.children.find {
        case t => t.pos.includes(selection.pos)
      }.map(_.pos.start).getOrElse(selection.pos.start)
      !pos.isRange || pos.start < startOfTopLevelTree
    }

    private def isBeforeEndOfSelection(pos: Position) = {
      !pos.isRange || pos.start < selection.pos.end
    }

    /**
     * Inserts trees in the enclosing block right before the selection.
     */
    lazy val beforeSelectionInBlock: InsertionPosition = {
      case enclosing @ Block(stats, expr) =>
        InsertionPoint(enclosing, { insertion =>
          mkBlock(insertInSeq(stats :+ expr, insertion, isBeforeSelectionIn(enclosing)))
        })
    }

    /**
     * Inserts trees in the enclosing template right after the selection.
     */
    lazy val afterSelectionInTemplate: InsertionPosition = {
      case enclosing @ Template(_, _, body) =>
        InsertionPoint(enclosing, { insertion =>
          enclosing copy (body = insertInSeq(body, insertion, isBeforeEndOfSelection))
        })
    }
  }
}