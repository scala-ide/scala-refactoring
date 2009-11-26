package scala.tools.refactor.printer

import scala.tools.nsc.ast.Trees

object SourceHelper {
  
  def indentationLength(f: Fragment): Int = f match {
    case f: OriginalSourceFragment => try {
        indentationLength(f.start, f.file.content)
    } catch {
      case _: UnsupportedOperationException => 0
      case e => throw e
    }
    case _ => 0
  }

  def indentationLength(tree: Trees#Tree): Int = {
    indentationLength(tree.pos.start, tree.pos.source.content)
  }
  
  def indentationLength(start: Int, content: Array[Char]) = {
    var i = if(start == content.length) start - 1 else start
        
    while(i >= 0 && content(i) != '\n')
      i -= 1
    i += 1
        
    val indentation = """\s*""".r.findFirstIn(content.slice(i, start)).getOrElse("")
        
    indentation.length
  }
}
