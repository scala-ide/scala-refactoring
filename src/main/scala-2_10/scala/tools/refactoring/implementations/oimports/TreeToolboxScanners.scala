package scala.tools.refactoring
package implementations.oimports

import scala.annotation.tailrec
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Global
import scala.util.Properties

class TreeToolboxScanners[G <: Global](val global: G) {
  import scala.collection._
  import global.syntaxAnalyzer._

  class CommentScanner(source: SourceFile) extends SourceFileScanner(source) {
    private val comments_ = mutable.ListBuffer[RangePosition]()
    override protected def foundComment(value: String, start: Int, end: Int): Unit =
      comments_ += new RangePosition(source, start, start, end + 2)

    def scan(): Unit = {
      init()
      import scala.tools.nsc.ast.parser.Tokens.EOF
      @tailrec def scan(): Unit = if (token == EOF) () else { nextToken(); scan() }
      scan()
    }

    def comments = comments_.toList
  }
}

