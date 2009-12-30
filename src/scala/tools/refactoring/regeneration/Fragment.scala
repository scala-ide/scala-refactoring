package scala.tools.refactoring.regeneration

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags
import scala.collection.mutable.ListBuffer

/*
 * Terminology: Layout instead of Layout? 
 * */

trait WithTree {
  def tree: Trees#Tree
}

abstract class Fragment extends WithRequisite {
  val isLayout = false
  val isEndOfScope = false
  val isBeginOfScope = false
  def print: String
  override def toString = print
}

trait OriginalSourceFragment {
  def start: Int
  def end: Int
  def file: SourceFile
}

case class LayoutFragment(val start: Int, val end: Int, file: SourceFile) extends Fragment with OriginalSourceFragment {
  override val isLayout = true
  def print = new String(file.content.slice(start, end))
  override def toString = if(start == end) "❒" else new String(file.content.slice(start, end))
}

case class StringFragment(string: String) extends Fragment {
  val print = string
}

abstract class Scope extends Fragment {
  val parent: Option[Scope]
  def relativeIndentation: Int
  def children: List[Fragment]
  def lastChild: Option[Fragment] = if(trueChildren.isEmpty) None else Some(trueChildren.last)
  def indentation: Int = parent match {
    case Some(fragment) => fragment.indentation + relativeIndentation
    case None => relativeIndentation
  }
  protected val trueChildren = new ListBuffer[Fragment]()
  def add(p: Fragment) = trueChildren += p //assert that they are in order?
  def print = children mkString
  override def toString = "→"+ indentation +"("+ relativeIndentation +")"+ (children mkString "|")
}

case class SimpleScope(parent: Option[Scope], relativeIndentation: Int) extends Scope {
  
  object beginOfScope extends StringFragment("❨") {
    override val isBeginOfScope = true 
  }
  object endOfScope extends StringFragment("❩") {
    override val isEndOfScope = true 
  }
  
  def children = beginOfScope :: trueChildren.toList ::: endOfScope :: Nil
}

case class TreeScope(parent: Option[Scope], start: Int, end: Int, file: SourceFile, relativeIndentation: Int, tree: Trees#Tree) extends Scope with OriginalSourceFragment with WithTree {

  class BeginOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: TreeScope) extends Fragment with OriginalSourceFragment {
    override def toString = "❨"
    override val isBeginOfScope = true 
    val print = ""
    override def equals(that: Any) = that match {
      case that: TreeScope#BeginOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
      case _ => false
    }
  }
  
  class EndOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: TreeScope) extends Fragment with OriginalSourceFragment {
    override val toString = "❩"
    override val isEndOfScope = true 
    val print = ""
    override def equals(that: Any) = that match {
      case that: TreeScope#EndOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
      case _ => false
    }
  }
  
  private val beginOfScope = new BeginOfScope(start, start, file, this)
  private val endOfScope = new EndOfScope(end, end, file, this)
  
  def children: List[Fragment] = beginOfScope :: trueChildren.toList ::: endOfScope :: Nil
  // would it be enough to just check whether the trees are equal?
  override def equals(that: Any) = that match {
    case that: TreeScope => that.start == this.start && that.end == this.end && that.file == this.file// && that.parent == this.parent
    case _ => false
  }
}

case class SymTreeFragment(tree: Trees#SymTree) extends Fragment with OriginalSourceFragment with WithTree {
  override def hashCode = toString.hashCode + start * 31 * (end + 17)
  override def equals(that: Any) = that match {
    case that: SymTreeFragment => that.start == this.start && that.end == this.end && that.file == this.file
    case _ => false
  }
  val start = tree.pos.point
  val end = tree.pos.point + tree.symbol.nameString.length
  val file = tree.pos.source.asInstanceOf[BatchSourceFile]
  def print = new String(file.content.slice(start, end))
}

case class ArtificialTreeFragment(tree: Trees#Tree) extends Fragment with WithTree {
  def print = "?"+ tree.getClass.getSimpleName
}

case class TreeFragment(tree: Trees#Tree) extends Fragment with OriginalSourceFragment with WithTree {
  val start = tree.pos.start
  val end = tree.pos.end
  val file = tree.pos.source
  def print = new String(file.content.slice(start, end))
}

case class FlagFragment(flag: Long, pos: Position) extends Fragment with OriginalSourceFragment {
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
    case PARAM        => ""
    case Tokens.VAL   => "val"
    case Tokens.TYPE  => "type"
    case Tokens.DEF   => "def"
    case _            => "<unknown>: " + flagsToString(flag)
  }
}