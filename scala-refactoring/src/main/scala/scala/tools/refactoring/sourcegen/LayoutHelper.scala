package scala.tools.refactoring
package sourcegen

import tools.nsc.util.RangePosition
import tools.nsc.util.SourceFile

trait LayoutHelper {
  
  self: Formatting with common.Tracing with common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
  
  trait Layout {
    override def toString() = "ø"
  }
    
  case class LayoutFromFile(source: SourceFile, start: Int, end: Int) extends Layout {

    override def toString() = source.content.slice(start, end) mkString
    
    def splitAt(ch: Char) = {
      if(toString.indexOf(ch) >= 0) {
        val i = start + toString.indexOf(ch) + 1
        copy(end = i) →  copy(start = i)
      }
      else
        this → this
    }
    
    def splitBefore(ch: Char) = {
      if(toString.indexOf(ch) >= 0) {
        val i = start + toString.indexOf(ch)
        copy(end = i) →  copy(start = i)
      }
      else
        this → this
    }
        
    def splitAt(i: Int) = LayoutFromFile(source, start, i) → LayoutFromFile(source, i, end)
  }
  
  case class LayoutFromString(override val toString: String) extends Layout
  
  case object NoLayout extends Layout {
    override def toString() = ""
  }
  
  def layout(start: Int, end: Int)(implicit s: SourceFile) = LayoutFromFile(s, start, end)
  def between(l: Tree, r: Tree)(implicit s: SourceFile) = layout(l.pos.end, r.pos.start)(s)

  def layoutForCuRoot(t: Tree): (Layout, Layout) = 
    LayoutFromFile(t.pos.source, 0, t.pos.start) → 
    LayoutFromFile(t.pos.source, t.pos.end, t.pos.source.length)
    
  def layoutForSingleChild(t: Tree, p: Tree): (Layout, Layout) = 
    splitLayoutBetweenParentAndFirstChild(child = t, parent = p)._2 →     
    splitLayoutBetweenLastChildAndParent(child = t, parent = p)._1
    
  def layoutForLeftOuterChild(t: Tree, p: Tree, right: Tree): (Layout, Layout) = 
    splitLayoutBetweenParentAndFirstChild(t, p)._2 → 
    splitLayoutBetweenSiblings(left = t, right = right)._1 
    
  def layoutForRightOuterChild(t: Tree, p: Tree, left: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(left = left, right = t)._2  → 
    splitLayoutBetweenLastChildAndParent(child = t, parent = p)._1
    
  def layoutForEnclosedChild(t: Tree, left: Tree, right: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(left = left, right = right)

  def splitLayoutBetweenParentAndFirstChild(child: Tree, parent: Tree): (Layout, Layout) = {
    
    implicit val currentFile = child.pos.source
    
    (parent, child) match {
      
      case (p: PackageDef, c) =>
        layout(p.pos.start, c.pos.start) splitAt c.pos.start
        
      case (p: ClassDef, t) =>
        layout(p.pos.start, p.pos.point) → layout(p.pos.point + p.name.length, t.pos.start)
        
      case (p: Template, t) =>
        layout(p.pos.start, t.pos.start) splitAt '{'
        
      case (p: ValDef, t: ModifierTree) =>
        NoLayout → NoLayout
        
      case (p: ValDef, t) =>
        layout(p.pos.start, p.pos.point) → NoLayout
      
      case (p, t) => throw new Exception("Unhandled parent: "+ p.getClass.getSimpleName +", child: "+ t.getClass.getSimpleName)
    }
  }
  
   def splitLayoutBetweenLastChildAndParent(child: Tree, parent: Tree): (Layout, Layout) = {
     
     implicit val currentFile = child.pos.source
     
     (child, parent) match {
       
       case (c, p: PackageDef) =>
         layout(c.pos.end, p.pos.end) splitAt '\n'
         
       case (c, p: ClassDef) =>
         layout(c.pos.end, p.pos.end) splitAt '}'
         
       case (c, p: Template) =>
         layout(c.pos.end, p.pos.end) splitBefore '\n'
         
       case (c, p: ValDef) =>
         NoLayout → NoLayout
         
       
       case (c, p) => throw new Exception("Unhandled parent: "+ p.getClass.getSimpleName +", child: "+ c.getClass.getSimpleName)
     }
   }
 
  def  splitLayoutBetweenSiblings(left: Tree, right: Tree): (Layout, Layout) = {
    
    implicit val currentFile = left.pos.source
    
    def split(layout: String) = {
  
      val EmptyParens = """(.*?\(\s*\)\s*)(.*)""".r
      val OpeningBrace = """(.*?\()(.*)""".r
      val Colon = """(.*?)(:.*)""".r
      val Arrow = """(.*?=>\s?)(.*)""".r
      val Equals = """(.*?=\s?)(.*)""".r
      val ClosingBrace = """(?ms)(.*?)(\).*)""".r
      val Comma = """(.*?),\s?(.*)""".r
      val NewLine = """(?ms)(.*?)(\n.*)""".r
      val ImportStatementNewline = """(?ms)(.*)(\n.*?import.*)""".r // imports don't include leading lines, handle in partitioner instead?
      val ImportStatement = """(?ms)(.*)(.*?import.*)""".r
      
      (layout match {
        case Colon(l, r)           => Some(l, r, "Colon")
        case EmptyParens(l, r)     => Some(l, r, "EmptyParens")
        case OpeningBrace(l, r)    => Some(l, r, "OpeningBrace")
        case Arrow(l, r)           => Some(l, r, "Arrow")
        case Equals(l, r)          => Some(l, r, "Equals")
        case ClosingBrace(l, r)    => Some(l, r, "ClosingBrace")
        case ImportStatementNewline(l, r) => Some(l, r, "ImportStatement Newline")
        case _                     => None
      }) orElse (layout match { // Work around https://lampsvn.epfl.ch/trac/scala/ticket/1133
        case ImportStatement(l, r) => Some(l, r, "ImportStatement")
        case NewLine(l, r)         => Some(l, r, "NewLine")
        case Comma(l, r)           => Some(l, r, "Comma")
        case s                     => Some(s, "", "NoMatch")
      }) get
    }
    
    
    (left, right) match {
      case (_, EmptyTree) | (EmptyTree, _) => LayoutFromString("") → LayoutFromString("")
      case (l, r) => 
        val (ll, lr, rule) = split(between(l, r).toString)
        trace("Rule %s splits layout into %s and %s", rule, ll, lr)
        LayoutFromString(ll) → LayoutFromString(lr)
    }
    
  }
}