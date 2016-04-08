package scala.tools.refactoring
package implementations.oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global

class TreeToolboxScanners[G <: Global](val global: G) {
  import global.syntaxAnalyzer._
  import scala.collection._

  class CommentScanner(source: SourceFile) extends SourceFileScanner(source) {
    private val comments_ = mutable.ListBuffer[RangePosition]()
    override def skipComment(): Boolean = {
      val start = offset
      val result = super.skipComment()
      if (result) {
        comments_ += new RangePosition(source, start, start, charOffset)
      }
      result
    }

    def scan(): Unit = {
      init()
      import scala.tools.nsc.ast.parser.Tokens.EOF
      @tailrec def scan(): Unit = if (token == EOF) () else { nextToken(); scan() }
      scan()
    }

    def comments = comments_.toList
  }
}
