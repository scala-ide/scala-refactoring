package scala.tools.refactoring
package sourcegen

import tools.nsc.util.RangePosition
import tools.nsc.util.SourceFile

trait Requisite {
  self =>
  
  def isRequired(l: Layout, r: Layout): Boolean
  
  def validate(l: Layout, r: Layout): Layout = {
    if(isRequired(l, r)) {
      l ++ getLayout ++ r
    } else {
      l ++ r
    }
  }
  
  protected def getLayout: Layout
  
  def ++(other: Requisite): Requisite = (self, other) match {
    case (_1, NoRequisite) => _1
    case (NoRequisite, _2) => _2
    case _ => new Requisite {
      def isRequired(l: Layout, r: Layout) = self.isRequired(l, r) || other.isRequired(l, r)
      def getLayout = self.getLayout ++ other.getLayout
      override def validate(l: Layout, r: Layout) = {
        val _1 = if(self.isRequired(l, r)) self.getLayout else NoLayout
        val _2 = if(other.isRequired(l, r)) other.getLayout else NoLayout
        l ++ _1 ++ _2 ++ r
      }
    }
  }
}

object Requisite {
    
  def allowSurroundingWhitespace(regex: String) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      !l.matches(".*"+ regex +"\\s*$") && !r.matches("^\\s*"+ regex + ".*")
    }
    def getLayout = Layout(regex.replace("\\", ""))
  }
  
  def anywhere(s: String) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      !(l.contains(s) || r.contains(s))
    }
    def getLayout = Layout(s)
  }
  
  val Blank = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      val _1 = l.matches(".*\\s+$")
      val _2 = r.matches("^\\s+.*")
      
      !(_1 || _2)
    }
    val getLayout = Layout(" ")
  }
  
  def newline(indentation: String) = new Requisite {
    def isRequired(l: Layout, r: Layout) = {
      val _1 = l.matches("(?ms).*\n\\s*$")
      val _2 = r.matches("(?ms)^\\s*\n.*")
      !(_1 || _2)
    }
    def getLayout = Layout("\n"+ indentation)
  }
}

object NoRequisite extends Requisite {
  def isRequired(l: Layout, r: Layout) = false
  val getLayout = NoLayout
}

trait Fragment {
  self =>
  val leading:  Layout
  val center:   Layout
  val trailing: Layout
  
  val pre  = NoRequisite: Requisite
  val post = NoRequisite: Requisite
  
  override def toString() = asText
  
  def toLayout = new Layout {
    def asText = self.asText
  }
  
  def asText: String = pre.validate(NoLayout, leading).asText + center.asText + post.validate(trailing, NoLayout).asText
  
  /**
   * Combines two fragments, makes sure that 
   * Requisites are satisfied.
   * 
   * Combining two fragments (a,b,c) and (d,e,f)
   * yields a fragment (a,bcde,f).
   * */
  def ++ (o: Fragment): Fragment = o match {
    case EmptyFragment => this
    case _ => new Fragment {
      val leading  = self.leading
      val center   = self.center ++  (self.post ++ o.pre).validate(self.trailing, o.leading)  /*self.post.validate(o.pre.validate(self.trailing, o.leading), NoLayout)*/ ++ o.center
      val trailing = o.trailing
      
      override val pre  = self.pre
      override val post = o.post
    }
  }
  
  /**
   * Combines a fragment with a layout, makes sure that 
   * Requisites are satisfied.
   * 
   * Combining (a,b,c) and (d)
   * yields a fragment (a,b,cd).
   * */
  def ++ (o: Layout): Fragment = o match {
    case NoLayout => this
    case _ => new Fragment {
      val leading  = self.leading
      val center   = self.center
      val trailing = self.post.validate(self.trailing, o)
      
      override val pre  = self.pre
      override val post = /*if (self.post.isRequired(this.trailing, NoLayout)) self.post else*/ NoRequisite
    }
  }
  
  def ++ (after: Requisite, before: Requisite = NoRequisite): Fragment = {
    new Fragment {
      val leading  = self.leading
      val center   = self.center
      val trailing = self.trailing
      
      override val pre  = before ++ self.pre
      override val post = self.post ++ after
    }
  }
}

abstract class EmptyFragment extends Fragment {
  val leading  = NoLayout: Layout
  val center   = NoLayout: Layout
  val trailing = NoLayout: Layout
}

object EmptyFragment extends EmptyFragment

object Fragment {
  
  def unapply(f: Fragment) = Some(f.leading, f.center, f.trailing)
  
  def apply(l: Layout, c: Layout, t: Layout) = new Fragment {
    val leading = l
    val center = c
    val trailing = t
  }
  
  def apply(s: String) = new EmptyFragment {
    override val center = Layout(s)
  }
}

trait Layout extends CommentHelpers {
  self =>
  
  def contains(s: String) = stripComment(asText).contains(s)
  
  def matches(r: String) = stripComment(asText).matches(r)

  def asText: String
  
  override def toString() = asText
  
  def ++ (o: Layout) = o match {
    case NoLayout => this 
    case _ => new Layout {
      override def asText = self.asText + o.asText
    }
  }
  
  def ++ (o: Fragment): Fragment = new Fragment {
    val leading  = o.pre.validate(self, o.leading)
    val center   = o.center
    val trailing = o.trailing
    
    override val pre  = if (o.pre.isRequired(this.leading, NoLayout)) o.pre else NoRequisite
    override val post = o.post    
  }
  
  def ++ (r: Requisite): Fragment = new EmptyFragment {
    override val trailing = self
    override val post = r
  }
}

case object NoLayout extends Layout {
  val asText = ""
}

object Layout {
  
  case class LayoutFromFile(source: SourceFile, start: Int, end: Int) extends Layout {
  
    lazy val asText = source.content.slice(start, end) mkString
          
    def splitAfter(cs: Char*): (Layout, Layout) = split(cs) match {
      case None => this → NoLayout
      case Some(i) => copy(end = i+1) → copy(start = i+1)
    }
    
    def splitBefore(cs: Char*): (Layout, Layout) = split(cs) match {
      case None => NoLayout → this
      case Some(i) => copy(end = i) →  copy(start = i)
    }
    
    private def split(cs: Seq[Char]): Option[Int] = cs.toList match {
      case Nil => 
        None
      case x :: xs =>
        val i = stripComment(asText).indexOf(x) 
        if(i >= 0 ) {
          Some(start + i)
        } else
          split(xs)
    }
  }
  
  def apply(source: SourceFile, start: Int, end: Int) = LayoutFromFile(source, start, end)
  
  def apply(s: String) = new Layout {
    val asText = s
  }
}
