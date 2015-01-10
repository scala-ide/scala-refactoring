package scala.tools.refactoring.common

import scala.tools.nsc.Global

/*
 * FIXME: This class duplicates functionality from org.scalaide.core.compiler.CompilerApiExtensions.
 */
trait CompilerApiExtensions {
  this: CompilerAccess =>
  import global._

  /** Locate the smallest tree that encloses position.
   *
   *  @param tree The tree in which to search `pos`
   *  @param pos  The position to look for
   *  @param p    An additional condition to be satisfied by the resulting tree
   *  @return     The innermost enclosing tree for which p is true, or `EmptyTree`
   *              if the position could not be found.
   */
  def locateIn(tree: Tree, pos: Position, p: Tree => Boolean = t => true): Tree =
    new FilteringLocator(pos, p) locateIn tree

  def enclosingPackage(tree: Tree, pos: Position): Tree = {
    locateIn(tree, pos, _.isInstanceOf[PackageDef])
  }

  private class FilteringLocator(pos: Position, p: Tree => Boolean) extends Locator(pos) {
    override def isEligible(t: Tree) = super.isEligible(t) && p(t)
  }
}
