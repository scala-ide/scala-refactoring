package scala.tools.refactor.printer

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait Part {
  val isWhiteSpace = false
  def print(out: Appendable)
  override def toString = {
    val sb = new java.lang.StringBuilder
    print(sb)
    sb.toString
  }
}

object nullPart extends WhiteSpacePart(0, 0, null) {
  override def print(out: Appendable) = ()
}

class WhiteSpacePart(start: Int, end: Int, file: SourceFile) extends Part {
  override val isWhiteSpace = true
  def print(out: Appendable) {
    file.content.slice(start, end).foreach(out append _)
  }
  
  def offset(o: Int) = new WhiteSpacePart(start + o, end, file)
}

class SymbolPart(tree: Trees#SymTree) extends Part {
  def print(out: Appendable) {
    val sym = tree.symbol
    val src = tree.pos.source.asInstanceOf[BatchSourceFile]
    src.content.slice(tree.pos.point, tree.pos.point + sym.nameString.length).foreach(out append _)
  }
}

class FlagPart(flag: Long, pos: Position) extends Part {
  import Flags._
  def print(out: Appendable) = out append(flag match {
    case TRAIT        => "trait"
    case FINAL        => "final"
    case IMPLICIT     => "implicit"
    case PRIVATE      => "private"
    case PROTECTED    => "protected"
    case SEALED       => "sealed"
    case OVERRIDE     => "override"
    case CASE         => "case"
    case ABSTRACT     => "abstract"
    case Tokens.VAL   => "val"
    case _            => "<unknown>: " + flagsToString(flag)
  })
}