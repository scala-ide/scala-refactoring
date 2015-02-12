package scala.tools.refactoring.common

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.ast.parser.Tokens

/*
 * FIXME: This class duplicates functionality from [[org.scalaide.core.compiler.CompilerApiExtensions]].
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

  /*
   * For Scala-2.10 (see scala.reflect.internal.Positions.Locator in Scala-2.11).
   */
  private class Locator(pos: Position) extends Traverser {
    var last: Tree = _
    def locateIn(root: Tree): Tree = {
      this.last = EmptyTree
      traverse(root)
      this.last
    }
    protected def isEligible(t: Tree) = !t.pos.isTransparent
    override def traverse(t: Tree) {
      t match {
        case tt : TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
          traverse(tt.original)
        case _ =>
          if (t.pos includes pos) {
            if (isEligible(t)) last = t
            super.traverse(t)
          } else t match {
            case mdef: MemberDef =>
              traverseTrees(mdef.mods.annotations)
            case _ =>
          }
      }
    }
  }

  /** A helper class to access the lexical tokens of `source`.
   *
   *  Once constructed, instances of this class are thread-safe.
   */
  class LexicalStructure(source: SourceFile) {
    private val token = new ArrayBuffer[Int]
    private val startOffset = new ArrayBuffer[Int]
    private val endOffset = new ArrayBuffer[Int]
    private val scanner = new syntaxAnalyzer.UnitScanner(new CompilationUnit(source))
    scanner.init()

    while (scanner.token != Tokens.EOF) {
      startOffset += scanner.offset
      token += scanner.token
      scanner.nextToken
      endOffset += scanner.lastOffset
    }

    /** Return the index of the token that covers `offset`.
     */
    private def locateIndex(offset: Int): Int = {
      var lo = 0
      var hi = token.length - 1
      while (lo < hi) {
        val mid = (lo + hi + 1) / 2
        if (startOffset(mid) <= offset) lo = mid
        else hi = mid - 1
      }
      lo
    }

    /** Return all tokens between start and end offsets.
     *
     *  The first token may start before `start` and the last token may span after `end`.
     */
    def tokensBetween(start: Int, end: Int): immutable.Seq[Token] = {
      val startIndex = locateIndex(start)
      val endIndex = locateIndex(end)

      val tmp = for (i <- startIndex to endIndex)
        yield Token(token(i), startOffset(i), endOffset(i))

      tmp.toSeq
    }
  }

  /** A Scala token covering [start, end)
   *
   *  @param tokenId one of scala.tools.nsc.ast.parser.Tokens identifiers
   *  @param start   the offset of the first character in this token
   *  @param end     the offset of the first character after this token
   */
  case class Token(tokenId: Int, start: Int, end: Int)
}
