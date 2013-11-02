package scala.tools.refactoring.common

import scala.reflect.internal.util.RangePosition

trait ReplaceableSelections extends Selections {
  self: CompilerAccess =>

  import global._
  import PartialFunction._

  /**
   * Helper methods for expansion of selections to new selections
   * that encloses a specific statement or sequence of satements.
   */
  implicit class ExpandableSelection(selection: Selection) {
    def expandTo(newPos: Position): Option[Selection] = {
      if (newPos.isRange)
        Some(new Selection {
          val root = selection.root
          val file = selection.file
          val pos = newPos.asInstanceOf[RangePosition]
        })
      else
        None
    }

    def expandTo(tree: Tree): Option[Selection] =
      expandTo(tree.pos)

    def expandTo[T <: Tree](implicit m: Manifest[T]): Option[Selection] = {
      val newSelTree = selection.findSelectedOfType[T]
      newSelTree.flatMap(expandTo(_))
    }

    def expand: Option[Selection] = {
      selection.findSelectedOfType[Tree].flatMap { enclosing =>
        def posOfPartiallySelectedTrees(trees: List[Tree], newPos: Position = selection.pos): Position =
          trees match {
            case t :: rest if !t.pos.isRange || t.pos.end < selection.pos.start || t.pos.start > selection.pos.end =>
              posOfPartiallySelectedTrees(rest, newPos)
            case t :: rest =>
              posOfPartiallySelectedTrees(rest, newPos union t.pos)
            case Nil => newPos
          }

        expandTo(posOfPartiallySelectedTrees(enclosing.children))
      }
    }
  }

  implicit class ReplaceableSelection(selection: Selection) {

  }
}