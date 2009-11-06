package scala.tools.refactor.printer

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

trait Requirement {
  
}

trait Part {
  val isWhiteSpace = false
  val start: Int
  val end: Int
  def print(out: Appendable)
  override def toString = {
    val sb = new java.lang.StringBuilder
    print(sb)
    sb.toString
  }
}

case object BeginOfFile extends Part {
  val start = 0
  val end = 0
  override def toString = "❰"
  def print(out: Appendable) = ()
}

case class EndOfFile(file: SourceFile) extends Part {
  val start = file.length
  val end = file.length
  override def toString = "❱"
  def print(out: Appendable) = ()
}

case object nullPart extends WhiteSpacePart(0, 0, null) {
  override def print(out: Appendable) = ()
}

case class WhiteSpacePart(val start: Int, val end: Int, file: SourceFile) extends Part {
  override val isWhiteSpace = true
  def print(out: Appendable) {
    file.content.slice(start, end).foreach(out append _)
  }
  
  def offset(o: Int) = new WhiteSpacePart(start + o, end, file)
}

case class StringPart(string: String) extends Part {
  val start = -1
  val end = -1
  def print(out: Appendable) = string foreach(out append)
}

case class SymbolPart(tree: Trees#SymTree) extends Part {
  override def hashCode = toString.hashCode + start * 31 * (end + 17)
  override def equals(that: Any) = that match {
    case that: SymbolPart => that.start == this.start && that.end == this.end && that.src == this.src
    case _ => false
  }
  val end = tree.pos.point +  tree.symbol.nameString.length
  val start = tree.pos.point
  val src = tree.pos.source.asInstanceOf[BatchSourceFile]
  def print(out: Appendable) {
    src.content.slice(start, end).foreach(out append _)
  }
}

case class FlagPart(flag: Long, pos: Position) extends Part {
  val start = pos.start
  val end = pos.end
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