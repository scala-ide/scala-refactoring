package scala.tools.refactor.printer

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags
import scala.collection.mutable.ListBuffer

/*
 * Terminology: Layout instead of Whitespace? 
 * */


case class Required(check: String, write: String) {
  def this(check: String) = this(check, check)
  override def toString = check
}

trait WithRequirement {
  val postRequirements = new ListBuffer[Required]
  val preRequirements = new ListBuffer[Required]
  def requirePost(r: Required): this.type = {
    postRequirements += r
    this
  }
  def requirePre(r: Required): this.type = {
    preRequirements += r
    this
  }
  def hasRequirements = postRequirements.size > 0 || preRequirements.size > 0
  def copyRequirements(from: WithRequirement): this.type = {
    from.postRequirements foreach (requirePost _)
    from.preRequirements foreach (requirePre _)
    this
  }
}

trait WithTree {
  def tree: Trees#Tree
}

abstract sealed class Part extends WithRequirement {
  val isWhitespace = false
  val isEndOfScope = false
  val isBeginOfScope = false
  def print: String
  override def toString = print
}

trait OriginalSourcePart extends Part {
  def start: Int
  def end: Int
  def file: SourceFile
}

trait Whitespace

case class WhitespacePart(val start: Int, val end: Int, file: SourceFile) extends Part with OriginalSourcePart with Whitespace {
  override val isWhitespace = true
  def print = new String(file.content.slice(start, end))
  override def toString = if(start == end) "❒" else new String(file.content.slice(start, end))
}

case class StringPart(string: String) extends Part {
  val print = string
}

case class RequirementPart(string: String) extends Part {
  val print = string
}

abstract class ScopePart extends Part {
  val parent: Option[ScopePart]
  def relativeIndentation: Int
  def children: List[Part]
  def lastChild: Option[Part] = if(trueChildren.isEmpty) None else Some(trueChildren.last)
  def indentation: Int = parent match {
    case Some(part) => part.indentation + relativeIndentation
    case None => relativeIndentation
  }
  protected val trueChildren = new ListBuffer[Part]()
  def add(p: Part) = trueChildren += p //make sure they are in order
  def print = children mkString
  override def toString = "→"+ indentation +"("+ relativeIndentation +")"+ (children mkString "|")
}

case class SimpleScope(parent: Option[ScopePart], relativeIndentation: Int) extends ScopePart {
  
  object beginOfScope extends StringPart("❨") {
    override val isBeginOfScope = true 
  }
  object endOfScope extends StringPart("❩") {
    override val isEndOfScope = true 
  }
  
  def children = beginOfScope :: trueChildren.toList ::: endOfScope :: Nil
}

case class CompositePart(parent: Option[ScopePart], start: Int, end: Int, file: SourceFile, relativeIndentation: Int, tree: Trees#Tree) extends ScopePart with OriginalSourcePart with WithTree {

  class BeginOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: CompositePart) extends Part with OriginalSourcePart {
    override def toString = "❨"
    override val isBeginOfScope = true 
    val print = ""
    override def equals(that: Any) = that match {
      case that: CompositePart#BeginOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
      case _ => false
    }
  }
  
  class EndOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: CompositePart) extends Part with OriginalSourcePart {
    override val toString = "❩"
    override val isEndOfScope = true 
    val print = ""
    override def equals(that: Any) = that match {
      case that: CompositePart#EndOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
      case _ => false
    }
  }
  
  private val beginOfScope = new BeginOfScope(start, start, file, this)
  private val endOfScope = new EndOfScope(end, end, file, this)
  
  def children: List[Part] = beginOfScope :: trueChildren.toList ::: endOfScope :: Nil
  // would it be enough to just check whether the trees are equal?
  override def equals(that: Any) = that match {
    case that: CompositePart => that.start == this.start && that.end == this.end && that.file == this.file
    case _ => false
  }
}

case class SymTreePart(tree: Trees#SymTree) extends Part with OriginalSourcePart with WithTree {
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

case class ArtificialTreePart(tree: Trees#Tree) extends Part with WithTree {
  def print = "?"+ tree.getClass.getSimpleName
}

case class TreePart(tree: Trees#Tree) extends Part with OriginalSourcePart with WithTree {
  val start = tree.pos.start
  val end = tree.pos.end
  val file = tree.pos.source
  def print = new String(file.content.slice(start, end))
}

// add requirements here?
case class FlagPart(flag: Long, pos: Position) extends Part with OriginalSourcePart {
  lazy val start = pos.start
  lazy val end = start + print.length
  lazy val file = pos.source
  import Flags._
  def print = flag match {
    case 0            => ""
    case TRAIT        => "trait"
    case METHOD       => "def"
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