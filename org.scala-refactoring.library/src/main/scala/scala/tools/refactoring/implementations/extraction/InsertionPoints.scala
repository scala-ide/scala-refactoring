package scala.tools.refactoring.implementations.extraction

import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.Selections
import scala.tools.refactoring.transformation.TreeTransformations

trait InsertionPoints extends Selections with TreeTransformations { self: CompilerAccess =>
  import global._

  /**
   * An insertion point is a function that may be defined for
   * an enclosing tree. When defined, it returns an instance of
   * an insertion function, that takes a tree and includes it
   * in the enclosing tree at the insertion position.
   */
  type InsertionPoint = PartialFunction[Tree, Tree => Tree]

  private def insertInSeq(stats: List[Tree], insertion: Tree, isBeforeInsertionPoint: Position => Boolean) = {
    val (before, after) = stats.span((t: Tree) => isBeforeInsertionPoint(t.pos))
    before ::: insertion :: after ::: Nil
  }

  def atBeginningOfDefDef: InsertionPoint = {
    case enclosing @ DefDef(_, _, _, _, _, Block(stats, expr)) =>
      (insertion: Tree) =>
        enclosing copy (rhs = mkBlock(insertInSeq(stats :+ expr, insertion, _ => false)))
        case enclosing @ DefDef(_, _, _, _, _, rhs) =>
      (insertion: Tree) =>
        enclosing copy (rhs = mkBlock(insertion :: rhs :: Nil))
  }

  def atBeginningOfFunction: InsertionPoint = {
    case enclosing @ Function(_, Block(stats, expr)) =>
      (insertion: Tree) =>
        enclosing copy (body = mkBlock(insertInSeq(stats :+ expr, insertion, _ => false)))
        case enclosing @ Function(_, body) =>
      (insertion: Tree) =>
        enclosing copy (body = mkBlock(insertion :: body :: Nil))
  }

  def atEndOfParameterList: InsertionPoint = ???

  implicit class SelectionDependentInsertionPoints(selection: Selection) {    
    private def isBeforeSelectionIn(enclosing: Tree)(pos: Position) = {
      val startOfTopLevelTree = enclosing.children.find{
        case t => t.pos.includes(selection.pos)
      }.map(_.pos.start).getOrElse(0)
      !pos.isRange || pos.start < startOfTopLevelTree
    }

    private def isBeforeEndOfSelection(pos: Position) = {
      !pos.isRange || pos.start < selection.pos.end
    }

    def beforeSelectionInBlock: InsertionPoint = {
      case enclosing @ Block(stats, expr) =>
        (insertion: Tree) =>
          mkBlock(insertInSeq(stats :+ expr, insertion, isBeforeSelectionIn(enclosing)))
    }

    def afterSelectionInTemplate: InsertionPoint = {
      case enclosing @ Template(_, _, body) =>
        (insertion: Tree) =>
          enclosing copy (body = insertInSeq(body, insertion, isBeforeEndOfSelection))
    }
  }
}