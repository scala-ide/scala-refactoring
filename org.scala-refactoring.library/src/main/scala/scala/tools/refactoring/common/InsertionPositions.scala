package scala.tools.refactoring.common

import scala.tools.refactoring.transformation.TreeTransformations

trait InsertionPositions extends Selections with TreeTransformations { self: CompilerAccess =>
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
  case class InsertionPoint(enclosing: Tree, mkEnclosing: Tree => Tree, pos: Position) extends (Tree => Tree) {
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
  lazy val atBeginningOfNewDefBody: InsertionPosition = {
    case enclosing @ DefDef(_, _, _, _, _, NoBlock(rhs)) =>
      InsertionPoint(enclosing, { insertion: Tree =>
        enclosing copy (rhs = mkBlock(insertion :: rhs :: Nil))
      }, rhs.pos)
    case enclosing @ DefDef(_, _, _, _, _, rhs) if isSyntheticBlock(rhs) =>
      InsertionPoint(enclosing, { insertion: Tree =>
        enclosing copy (rhs = mkBlock(insertion :: rhs :: Nil))
      }, rhs.pos)
  }

  /**
   * Inserts trees as the first statement in a function body.
   */
  lazy val atBeginningOfNewFunctionBody: InsertionPosition = {
    case enclosing @ Function(_, NoBlock(body)) if enclosing.pos.isOpaqueRange =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (body = mkBlock(insertion :: body :: Nil))
      }, body.pos)
    case enclosing @ Function(_, body) if isSyntheticBlock(body) =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (body = mkBlock(insertion :: body :: Nil))
      }, body.pos)
  }

  /**
   * Inserts trees as the first statement in a case body (rhs).
   */
  lazy val atBeginningOfCaseBody: InsertionPosition = {
    case enclosing @ CaseDef(_, _, body) =>
      InsertionPoint(enclosing, { insertion =>
        enclosing copy (body = mkBlock(insertion :: body :: Nil))
      }, body.pos)
  }

  def isSyntheticBlock(t: Tree) = t match {
    case Block((v: ValDef) :: Nil, _) if v.symbol.isSynthetic => true
    case _ => false
  }

  /**
   * Inserts trees as additional method parameters.
   */
  lazy val atEndOfParameterList: InsertionPosition = ???

  implicit class SelectionDependentInsertionPoints(selection: Selection) {
    /**
     * Inserts trees in the enclosing block right before the selection.
     */
    lazy val beforeSelectionInBlock: InsertionPosition = {
      case enclosing @ Block(stats, expr) if !isSyntheticBlock(enclosing) =>
        InsertionPoint(enclosing, { insertion =>
          mkBlock(insertInSeq(stats :+ expr, insertion, isBeforeSelectionIn(enclosing)))
        }, posOfSelectedTreeIn(enclosing))
    }

    /**
     * Inserts trees in the enclosing template right after the selection.
     */
    lazy val afterSelectionInTemplate: InsertionPosition = {
      case enclosing @ Template(_, _, body) =>
        InsertionPoint(enclosing, { insertion =>
          enclosing copy (body = insertInSeq(body, insertion, isBeforeEndOfSelection))
        }, posOfSelectedTreeIn(enclosing))
    }

    /**
     * Inserts trees in the enclosing template right before the selection.
     */
    lazy val beforeSelectionInTemplate: InsertionPosition = {
      case enclosing @ Template(_, _, body) =>
        InsertionPoint(enclosing, { insertion =>
          enclosing copy (body = insertInSeq(body, insertion, isBeforeSelectionIn(enclosing)))
        }, posOfSelectedTreeIn(enclosing))
    }

    private def posOfSelectedTreeIn(enclosing: Tree) = {
      enclosing.children.filter((t: Tree) => isBeforeEndOfSelection(t.pos)).lastOption
        .map(_.pos).getOrElse(selection.pos)
    }

    private def isBeforeSelectionIn(enclosing: Tree)(pos: Position) = {
      val startOfTopLevelTree = enclosing.children.find {
        case t => t.pos.includes(selection.pos)
      }.map(_.pos.start).getOrElse(selection.pos.start)
      !pos.isRange || pos.start < startOfTopLevelTree
    }

    private def isBeforeEndOfSelection(pos: Position) = {
      !pos.isRange || pos.start < selection.pos.end
    }
  }
}