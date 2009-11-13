package scala.tools.refactor.printer

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags
import scala.collection.mutable.ListBuffer

trait WithRequirement {
  val postRequirements = new ListBuffer[String]
  def requirePost(text: String) {
    postRequirements += text
  }
  def hasRequirements = postRequirements.size > 0
}

abstract sealed class Part {
  val isWhitespace = false
  def print: String
  override def toString = print
}

trait OriginalSourcePart extends Part {
  def start: Int
  def end: Int
  def file: SourceFile
}

case object NullPart extends Part {
  def file = throw new Exception("No File in NullPart")
  def end = throw new Exception("No End in NullPart")
  def start = throw new Exception("No Start in NullPart")
  def print = throw new Exception("Can't print NullPart")
}

case class WhitespacePart(val start: Int, val end: Int, file: SourceFile) extends Part with OriginalSourcePart {
  override val isWhitespace = true
  def print = new String(file.content.slice(start, end))
  override def toString = if(start == end) "❒" else new String(file.content.slice(start, end))
}

case class StringPart(string: String) extends Part {
  val print = string
}

case class CompositePart(tree: Trees#Tree) extends Part with OriginalSourcePart with WithRequirement {
  
  class BeginOfScope(val start: Int, val end: Int, val file: SourceFile) extends Part with OriginalSourcePart {
    override def toString = "❨"
    val print = ""
    override def equals(that: Any) = that match {
      case that: CompositePart#BeginOfScope => that.start == this.start && that.end == this.end && that.file == this.file
      case _ => false
    }
  }
  
  class EndOfScope(val start: Int, val end: Int, val file: SourceFile) extends Part with OriginalSourcePart {
    override val toString = "❩"
    val print = ""
    override def equals(that: Any) = that match {
      case that: CompositePart#EndOfScope => that.start == this.start && that.end == this.end && that.file == this.file
      case _ => false
    }
  }
  
  var start = tree.pos.start
  var end = tree.pos.end
  val file = tree.pos.source.asInstanceOf[BatchSourceFile]
  val trueChildren = new ListBuffer[Part]()
  def add(p: Part) = trueChildren += p //make sure they are in order
  def children: List[Part] = new BeginOfScope(start, start, file) :: trueChildren.toList ::: new EndOfScope(end, end, file) :: Nil
  override def toString = children mkString "|"
  def print = children mkString
  override def equals(that: Any) = that match {
    case that: CompositePart => that.start == this.start && that.end == this.end && that.file == this.file
    case _ => false
  }
}

case class SymTreePart(tree: Trees#SymTree) extends Part with OriginalSourcePart with WithRequirement {
  override def hashCode = toString.hashCode + start * 31 * (end + 17)
  override def equals(that: Any) = that match {
    case that: SymTreePart => that.start == this.start && that.end == this.end && that.file == this.file
    case _ => false
  }
  val start = tree.pos.point
  val end = tree.pos.point + tree.symbol.nameString.length
  val file = tree.pos.source.asInstanceOf[BatchSourceFile]
  def print = new String(file.content.slice(start, end))
}

case class LiteralPart(tree: Trees#Literal) extends Part with OriginalSourcePart {
  val start = tree.pos.start
  val end = tree.pos.end
  val file = tree.pos.source
  def print = new String(file.content.slice(start, end))
}

case class FlagPart(flag: Long, pos: Position) extends Part with OriginalSourcePart {
  val start = pos.start
  val end = start + print.length
  val file = pos.source
  import Flags._
  def print = flag match {
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
    case Tokens.TYPE  => "type"
    case Tokens.DEF   => "def"
    case _            => "<unknown>: " + flagsToString(flag)
  }
}