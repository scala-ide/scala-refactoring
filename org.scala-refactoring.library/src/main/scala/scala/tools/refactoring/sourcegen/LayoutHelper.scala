/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import tools.nsc.interactive.Global
import tools.nsc.util.RangePosition
import tools.nsc.util.SourceFile
import common.Tracing
import common.PimpedTrees

trait LayoutHelper extends CommentHelpers {
  
  self: Formatting with Tracing with PimpedTrees =>
  
  val global: Global
  import global._
      
  def surroundingLayout(t: Tree) = findOriginalTree(t) map { t =>
  
    def layoutFromParent() = (t.originalLeftSibling, t.originalParent, t.originalRightSibling) match {
      case (_,          None,    _          ) => layoutForCompilationUnitRoot(t)        \\ (_ => trace("compilation unit root"))
      case (None,       Some(p), None       ) => layoutForSingleChild(t, p)             \\ (_ => trace("single child with parent %s", p.getClass.getSimpleName))
      case (None,       Some(p), Some(right)) => layoutForLeftOuterChild(t, p, right)   \\ (_ => trace("left outer child with parent %s", p.getClass.getSimpleName))
      case (Some(left), Some(p), None       ) => layoutForRightOuterChild(t, p, left)   \\ (_ => trace("right outer child with parent %s", p.getClass.getSimpleName))
      case (Some(left), Some(p), Some(right)) => layoutForEnclosedChild(t, left, right, parent = p) \\ (_ => trace("enclosed child"))
    }
    
    def layoutFromChildren() = children(t) match {
      case Nil =>
        NoLayout → NoLayout
      case c => 
        splitLayoutBetweenParentAndFirstChild(parent = t, child = c.head)._1 →
        splitLayoutBetweenLastChildAndParent (parent = t, child = c.last)._2
    }
    
    val (leadingLayoutFromParent, trailingLayoutFromParent) = layoutFromParent()
    val (leadingLayoutFromChild, trailingLayoutFromChild) = layoutFromChildren()
    
    trace("parent leading:  %s", leadingLayoutFromParent.toString)
    trace("parent trailing: %s", trailingLayoutFromParent.toString)
    trace("child leading:   %s", leadingLayoutFromChild.toString)
    trace("child trailing:  %s", trailingLayoutFromChild.toString)
    
    (leadingLayoutFromParent, leadingLayoutFromChild, trailingLayoutFromChild, trailingLayoutFromParent)
    
  } getOrElse (NoLayout, NoLayout, NoLayout, NoLayout)
  
  def layout(start: Int, end: Int)(implicit s: SourceFile) = Layout(s, start, end)
  def between(l: Tree, r: Tree)(implicit s: SourceFile) = layout(l.pos.end, r.pos.start)(s)

  def layoutForCompilationUnitRoot(t: Tree): (Layout, Layout) = 
    Layout(t.pos.source, 0, t.pos.start) → 
    Layout(t.pos.source, t.pos.end, t.pos.source.length)
    
  def layoutForSingleChild(t: Tree, p: Tree): (Layout, Layout) = 
    splitLayoutBetweenParentAndFirstChild(child = t, parent = p)._2 →     
    splitLayoutBetweenLastChildAndParent(child = t, parent = p)._1
    
  def layoutForLeftOuterChild(t: Tree, parent: Tree, right: Tree): (Layout, Layout) = 
    splitLayoutBetweenParentAndFirstChild(child = t, parent = parent)._2 → 
    splitLayoutBetweenSiblings(left = t, right = right, parent = parent)._1 
    
  def layoutForRightOuterChild(t: Tree, parent: Tree, left: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(left = left, right = t, parent = parent)._2  → 
    splitLayoutBetweenLastChildAndParent(child = t, parent = parent)._1
    
  def layoutForEnclosedChild(t: Tree, left: Tree, right: Tree, parent: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(parent = parent, left = left, right = t)._2 →
    splitLayoutBetweenSiblings(parent = parent, left = t, right = right)._1

  def splitLayoutBetweenParentAndFirstChild(child: Tree, parent: Tree): (Layout, Layout) = {
    
    trace("splitting layout between parent %s and first child %s", parent.getClass.getSimpleName, child.getClass.getSimpleName)
    
    implicit val currentFile = child.pos.source
    
    (fixValDefPosition(parent), fixValDefPosition(child)) match {
      
      case (p: PackageDef, c) =>
        layout(p.pos.start, c.pos.start) → NoLayout
        
      case (p @ ClassDef(ModifierTree(Nil), _, _, _), c) =>
        layout(p.pos.start,       p.pos.point) → layout(p.pos.point + p.name.length, c.pos.start)
        
      case (p @ ClassDef(ModifierTree(mods), _, _, _), c) =>
        layout(p.pos.start, mods.head.pos.start) → NoLayout
        
      case (p @ ModuleDef(ModifierTree(Nil), _, _), c) =>
        layout(p.pos.start,       p.pos.point) → layout(p.pos.point + p.name.length, c.pos.start)
        
      case (p @ ModuleDef(ModifierTree(mods), _, _), c) =>
        layout(p.pos.start, mods.head.pos.start) → NoLayout
        
      case (p @ TypeDef(ModifierTree(Nil), _, _, _), c) =>
        layout(p.pos.start,       p.pos.point) → layout(p.pos.point + p.name.length, c.pos.start)
        
      case (p @ TypeDef(ModifierTree(mods), _, _, _), c) =>
        layout(p.pos.start, mods.head.pos.start) → NoLayout
        
      case (p: Template, c) =>
        layout(p.pos.start, c.pos.start) splitAfter ('{', '(')
        
      case (p: If, c) =>
        layout(p.pos.start, c.pos.start) splitAfter '('
        
      case (p: ValOrDefDef, c: ModifierTree) =>
        NoLayout → NoLayout
        
      case (p: ValOrDefDef, c) =>
        layout(p.pos.start, p.namePosition.start) → NoLayout
        
      case (p: Apply, c) =>
        layout(p.pos.start, c.pos.start) → NoLayout
        
      case (p: Select, c) =>
        NoLayout → NoLayout
        
      case (p: Block, c) =>
        val l = layout(p.pos.start, c.pos.start)
        if(l contains "{")
          l splitAfter '{'
        else
          l splitBefore '\n'
         
      case (p: Try, c: Block) =>
        layout(p.pos.start, c.pos.start) splitBefore ('{')
         
      case (p: Try, c) =>
        layout(p.pos.start, c.pos.start) splitAfter ('\n', '{')
         
      case (p: Import, c) =>
        layout(p.pos.start, p.pos.point) → NoLayout
         
      case (p: ImportSelectorTree, c) =>
        layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: New, c) =>
        layout(p.pos.start + "new".length, c.pos.start) → NoLayout
         
      case (p, c) =>
        layout(p.pos.start, c.pos.start) → NoLayout
    }
  }
           
  private def fixValDefPosition(t: Tree): Tree = {
    
    t match {
      case t @ ValDef(_, _, _, rhs) =>
      
        val childBeforeRhs = children(t) takeWhile (c => !(c samePos rhs)) lastOption match {
          case Some(tree) => tree
          case None => return t
        }
      
        if(childBeforeRhs.pos.isRange && rhs.pos.isRange && between(childBeforeRhs, rhs)(t.pos.source).contains("{")) {
        
          val offsetToClosing = layout(rhs.pos.end, t.pos.source.length)(t.pos.source).asText takeWhile (_ != '}') length
          val ct = t.copy().copyAttrs(t)
        
          val end = t.pos.end + offsetToClosing + 1
          ct setPos (ct.pos withEnd end)
        } else {
          t
        }
      
      case _ => t
    }
  }

  def splitLayoutBetweenLastChildAndParent(child: Tree, parent: Tree): (Layout, Layout) = {
     
    trace("splitting layout between last child %s and parent %s", child.getClass.getSimpleName, parent.getClass.getSimpleName)
     
    implicit val currentFile = child.pos.source
     
    (fixValDefPosition(child), fixValDefPosition(parent)) match {
       
      case (c: Block, p) =>
        layout(c.pos.end, p.pos.end) splitAfter '}'
       
      case (c, p: PackageDef) =>
        layout(c.pos.end, p.pos.end) splitAfter '\n'
         
      case (c, p @ (_: ClassDef | _: ModuleDef)) =>
        layout(c.pos.end, p.pos.end) splitAfter '}'
         
       case (c: SuperConstructorCall, p: Template) =>
         layout(c.pos.end, p.pos.end) splitAtAndExclude ')'
         
       case (c, p: Template) =>
         layout(c.pos.end, p.pos.end) splitBefore (')', '\n')
         
       case (c, p: If) =>
         layout(c.pos.end, p.pos.end) splitBefore (')')
    
       case (c, p: ValOrDefDef) =>
         layout(c.pos.end, p.pos.end) splitAfterLast '}'
       
       case (c, p: Select) =>
         NoLayout → NoLayout
        
       case (c, p: Block) =>
         layout(c.pos.end, p.pos.end) splitAfter '\n'
         
       case (c, p: Match) =>
         layout(c.pos.end, p.pos.end) splitBefore ('\n')
         
       case (c, p) =>
         NoLayout → layout(c.pos.end, p.pos.end)
     }
   }
   
  private val Else = """(?ms)(.*?)(?:\n\s*\}\s*)?\n?\s*else\s*(?:[\s\{]*\n\s*)?(.*)""".r
  private val StartComment = """(.*?)(/\*.*)""".r
  private val Class = """(.*?)(class.*)""".r
  private val EmptyParens = """(?ms)(.*?\(\s*\)\s*)(.*)""".r
  private val OpeningBrace = """(.*?)\((.*)""".r
  private val OpeningCurlyBrace = """(?ms)(.*?)\{(.*)""".r
  private val Colon = """(.*?:\s+)(.*)""".r
  private val Arrow = """(.*?=>\s?)(.*)""".r
  private val Dot = """(.*)(\..*)""".r
  private val Equals = """(.*?=\s?)(.*)""".r
  private val ClosingBrace = """(?ms)(.*?)\)(.*)""".r
  private val ClosingCurlyBrace = """(?ms)(.*?\}\s*)(\n.*)""".r
  private val Comma = """(.*?),(.*)""".r
  private val NewLine = """(?ms)(.*?)(\n.*)""".r
  private val ImportStatementNewline = """(?ms)(.*)(\n.*?import.*)""".r
  private val ImportStatement = """(?ms)(.*)(.*?import.*)""".r
 
  def splitLayoutBetweenSiblings(parent: Tree, left: Tree, right: Tree): (Layout, Layout) = {
      
    def mergeLayoutWithComment(l: Seq[Char], c: Seq[Char]) = l zip c map {
      case (' ', _1) => _1
      case (_1, ' ') => _1
      case ('\n', '\n') => '\n'
    } mkString
    
    def split(layout: String): (String, String, String) = {

      (layout match {
        case Else(l, r)            => Some(l, r, "else")
        case StartComment(l, r)    => Some(l, r, "StartComment")
        case Class(l, r)           => Some(l, r, "Class")
        case Colon(l, r)           => Some(l, r, "Colon")
        case EmptyParens(l, r)     => Some(l, r, "EmptyParens")
        case OpeningBrace(l, r)    => Some(l, r, "OpeningBrace")
        case Arrow(l, r)           => Some(l, r, "Arrow")
        case _                     => None
      }) orElse (layout match { // Work around https://lampsvn.epfl.ch/trac/scala/ticket/1133
        case ClosingBrace(l, r)    => Some(l, r, "ClosingBrace")
        case Equals(l, r)          => Some(l, r, "Equals")
        case ImportStatementNewline(l, r) => Some(l, r, "ImportStatement Newline")
        case ImportStatement(l, r) => Some(l, r, "ImportStatement")
        case ClosingCurlyBrace(l, r)=> Some(l, r, "ClosingCurlyBrace")
        case NewLine(l, r)         => Some(l, r, "NewLine")
        case Comma(l, r)           => Some(l, r, "Comma")
        case Dot(l, r)             => Some(l, r, "Dot")
        case s                     => Some(s, "", "NoMatch")
      }) get
    }
    
    (fixValDefPosition(left), fixValDefPosition(right)) match {
      case (_, EmptyTree) | (EmptyTree, _) => NoLayout → NoLayout
      case (l: Import, r: Import) => NoLayout → NoLayout
        
      case (l, r: ImportSelectorTree) =>
        // All the layout, like '.' and '{' belongs to the selector.
        layout(l.pos.end, r.pos.start)(l.pos.source) → NoLayout

      case (l, r) => 
        
        val source = between(l, r)(left.pos.source).toString
        val (layout, comments) = splitComment(source)
        
        val (ll, lr, rule) = (l, parent, r) match {
            
          case (l: ValOrDefDef, _, r: ValOrDefDef) => 
            layout match {
              case Comma(l, r)   => (l, r, "Comma")
              case NewLine(l, r) => (l, r, "NewLine")
              case _ => split(layout)
            }
            
          case (l, parent: ValOrDefDef, NoBlock(r)) if r == parent.rhs && layout.contains("{") => 
            layout match {
              case OpeningCurlyBrace(l, r) => (l, "{"+ r, "OpeningCurlyBrace")
            }
            
          case (l, _, r) => split(layout)
        }
        
        trace("Rule %s splits (%s, %s) layout %s into %s and %s", rule, l.getClass.getSimpleName, r.getClass.getSimpleName, layout, ll, lr)
        Layout(mergeLayoutWithComment(ll, comments)) → Layout(mergeLayoutWithComment(lr reverse, comments reverse) reverse)
    }
  }
}