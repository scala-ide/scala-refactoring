package scala.tools.refactoring.regeneration

import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.util.Position
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags
import scala.collection.mutable.ListBuffer
import scala.collection.Traversable

trait Fragments {
  
  self: TreePrinter with FragmentRepository with SourceHelper =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  case class Requisite(check: String, write: String) {
    def this(check: String) = this(check, check)
    override def toString = check
  }
  
  trait WithRequisite {
    val requiredAfter = new ListBuffer[Requisite]
    val requiredBefore = new ListBuffer[Requisite]
    def requireAfter(r: Requisite): this.type = {
      requiredAfter += r
      this
    }
    def requireBefore(r: Requisite): this.type = {
      requiredBefore += r
      this
    }
    def hasRequirements = requiredAfter.size > 0 || requiredBefore.size > 0
  }

  trait WithTree {
    def tree: Tree
  }
  
  abstract class Fragment extends WithRequisite with Traversable[Fragment] {
    val isEndOfScope = false
    val isBeginOfScope = false
    def print: Seq[Char]
    override def toString = print mkString
    def foreach[U](f: Fragment => U): Unit = f(this)
    
    def render(fs: FragmentRepository): Seq[Char] = {
      this match {
        case f if fs exists f => f.print
        case f: FlagFragment => f.print
        case f: ImportSelectorsFragment => f.print
        case f: WithTree => renderTree(f)
        case f if f.isBeginOfScope || f.isEndOfScope  => ""
        case f => f.print
      }
    }
  }
  
  trait OriginalSourceFragment extends Fragment {
    def start: Int
    def end: Int
    def file: SourceFile
    override def print: Seq[Char] = file.content.slice(start, end)
    
    def layout(other: OriginalSourceFragment) = {
      assert(this.file == other.file)
      file.content.slice(end, other.start) mkString
    }
  }
  
  case class StringFragment(string: Seq[Char]) extends Fragment {
    val print = string
  }
  
  abstract class Scope extends Fragment {
    
    override def foreach[U](f: Fragment => U) = {
      f(this)
      children foreach (_ foreach f)
    }
    
    val parent: Option[Scope]
    def relativeIndentation: Int
    def children: List[Fragment]
    def lastChild: Option[Fragment] = trueChildren.lastOption
    def indentation: Int = relativeIndentation + (parent map (_.indentation) getOrElse(0))
    protected val trueChildren = new ListBuffer[Fragment]()
    def add(p: Fragment) = trueChildren += p //assert that they are in order?
    override def print: Seq[Char] = children mkString //?
    override def toString = {
      
      def expandChildrenWithLayout(c: List[Fragment]) = {
        (c zip c.tail flatMap {
          case (_1: OriginalSourceFragment, _2: OriginalSourceFragment) => 
            val l = _1.layout(_2)
            _1 :: (if(l == "") Nil else l :: Nil)
          case (_1, _) => 
            _1 :: Nil
        }) ::: c.last :: Nil
      }
      
      "→"+ indentation +"("+ relativeIndentation +")"+ (expandChildrenWithLayout(children) mkString "|")
    }
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
  
  case class TreeScope(parent: Option[Scope], start: Int, end: Int, file: SourceFile, relativeIndentation: Int, tree: Tree) extends Scope with OriginalSourceFragment with WithTree {
  
    class BeginOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: TreeScope) extends Fragment with OriginalSourceFragment {
      override def toString = "❨"
      override val isBeginOfScope = true 
      override val print = "": Seq[Char]
      override def equals(that: Any) = that match {
        case that: TreeScope#BeginOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
        case _ => false
      }
    }
    
    class EndOfScope(val start: Int, val end: Int, val file: SourceFile, val parent: TreeScope) extends Fragment with OriginalSourceFragment {
      override val toString = "❩"
      override val isEndOfScope = true 
      override val print = "": Seq[Char]
      override def equals(that: Any) = that match {
        case that: TreeScope#EndOfScope => that.start == this.start && that.end == this.end && that.file == this.file && that.parent == this.parent
        case _ => false
      }
    }
        
    private val beginOfScope = new BeginOfScope(start, start, file, this)
    private val endOfScope = new EndOfScope(end, end, file, this)
    
    def children: List[Fragment] = beginOfScope :: trueChildren.toList ::: endOfScope :: Nil
    
    override def hashCode = file.hashCode + start * 37 * (end + 13)
    override def equals(that: Any) = that match {
      case that: TreeScope => that.start == this.start && that.end == this.end && that.file == this.file
      case _ => false
    }
  }
  
  case class SymTreeFragment(tree: SymTree) extends Fragment with OriginalSourceFragment with WithTree {
    
    override def hashCode = file.hashCode + start * 31 * (end + 17)
    
    override def equals(that: Any) = /*PF*/that match {
      case that: SymTreeFragment => 
        that.start == this.start && 
        that.end == this.end && 
        that.file == this.file &&
        sameName(that)
      case _ => false
    }
    
    private def sameName(that: SymTreeFragment) = (this.tree, that.tree) match {
      case (_1: RefTree, _2: RefTree) => _1.name == _2.name
      case (_1: DefTree, _2: DefTree) => _1.name == _2.name
      case _ => true //we don't care about the name in that case
    }
    
    val start = tree.pos.point
    
    val end = tree.pos.point + (tree match {
      case t if t.symbol != NoSymbol => tree.symbol.nameString.length
      case t: DefTree => t.name.length
      case t => t.pos.end 
    })
    
    val file = tree.pos.source
  }
  
  case class ImportSelectorsFragment(selectors: List[ImportSelector], val file: SourceFile) extends Fragment with OriginalSourceFragment {
    val start = selectors.head.namePos
    val end = nameLength(selectors.last)
    override def print: Seq[Char] = {
      selectors map { s => file.content.slice(s.namePos, nameLength(s)) mkString } mkString ", "
    }
    private def nameLength(s: ImportSelector) = if(s.renamePos >= 0) s.renamePos + s.rename.length else s.namePos + s.name.length
  }
  
  case class ArtificialTreeFragment(tree: Tree) extends Fragment with WithTree {
    def print = "?"+ tree.getClass.getSimpleName
  }
  
  case class TreeFragment(tree: Tree) extends Fragment with OriginalSourceFragment with WithTree {
    lazy val start = tree.pos.start
    lazy val end = if(tree.pos.end + 1 == file.content.length) tree.pos.end + 1 else tree.pos.end
    lazy val file = tree.pos.source
  }
  
  trait Flag extends Fragment {
    def flag: Long
    import Flags._
    override lazy val print: Seq[Char] = flag match {
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
      case Tokens.VAR   => "var"
      case Tokens.TYPE  => "type"
      case Tokens.DEF   => "def"
      case _            => "<unknown>: " + flagsToString(flag)
    }
  }
  
  case class FlagFragment(flag: Long, pos: global.Position) extends Fragment with OriginalSourceFragment with Flag {
    lazy val start = pos.start
    lazy val end = start + print.length
    lazy val file = pos.source
  }
}