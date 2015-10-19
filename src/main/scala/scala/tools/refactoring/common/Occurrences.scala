package scala.tools.refactoring.common

import scala.tools.refactoring.analysis.Indexes
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.OffsetPosition

/**
 * Provides functionalities to get positions of term names. This includes the term name
 * defintion and all its uses.
 */
trait Occurrences extends Selections with CompilerAccess with Indexes {
  import global._

  type Occurrence = (Int, Int)

  private def termNameDefinition(root: Tree, name: String) = {
    root.collect {
      case t: DefTree if t.name.decode == name =>
        t
    }.headOption
  }

  private def defToOccurrence(t: DefTree) = t.namePosition() match {
    case p: RangePosition =>
      (p.start, p.end - p.start)
    case p: OffsetPosition =>
      (p.point, t.name.decode.length)
  }

  private def refToOccurrence(t: RefTree) = t.pos match {
    case p: RangePosition =>
      (p.start, p.end - p.start)
    case p: OffsetPosition =>
      (p.point, t.symbol.name.decode.length)
  }

  /**
   * Returns all uses of the term name introduced by the DefTree t.
   */
  def allOccurrences(t: DefTree): List[Occurrence] = {
    val refOccurrences = index.references(t.symbol).collect {
      case ref: RefTree => refToOccurrence(ref)
    }
    defToOccurrence(t) :: refOccurrences
  }

  /**
   * Searches for a definition of `name` in `root` and returns is's position
   * and all positions of references to the definition.
   * Returns an empty list if the definition of `name` is not in `selection`.
   */
  def termNameOccurrences(root: Tree, name: String): List[Occurrence] = {
    termNameDefinition(root, name) match {
      case Some(t) => allOccurrences(t)
      case None => Nil
    }
  }

  /**
   * Searches for a method definition of `defName` in `root` and returns for each
   * method parameter a list with the position of the parameter definition and
   * all occurrences.
   * Returns an empty list if `defName` is not found, defines something that is not
   * a method or if the method has no parameters.
   */
  def defDefParameterOccurrences(root: Tree, defName: String): List[List[Occurrence]] = {
    termNameDefinition(root, defName) match {
      case Some(DefDef(_, _, _, params, _, _)) =>
        params.flatten.map { p => allOccurrences(p) }
      case _ => Nil
    }
  }
}
