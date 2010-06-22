/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import tools.nsc.util.RangePosition
import tools.nsc.util.SourceFile

trait LayoutHelper extends CommentHelpers {
  
  self: Formatting with common.Tracing with common.PimpedTrees =>
  
  val global: scala.tools.nsc.interactive.Global
  import global._
      
  def surroundingLayout(t: Tree) = findOriginalTree(t) map { t =>
  
    def layoutFromParent() = (t.originalLeftSibling, t.originalParent, t.originalRightSibling) match {
      case (_,          None,    _          ) => layoutForCompilationUnitRoot(t)        \\ (_ => trace("compilation unit root"))
      case (None,       Some(p), None       ) => layoutForSingleChild(t, p)             \\ (_ => trace("single child with parent %s", p.getClass.getSimpleName))
      case (None,       Some(p), Some(right)) => layoutForLeftOuterChild(t, p, right)   \\ (_ => trace("left outer child with parent %s", p.getClass.getSimpleName))
      case (Some(left), Some(p), None       ) => layoutForRightOuterChild(t, p, left)   \\ (_ => trace("right outer child with parent %s", p.getClass.getSimpleName))
      case (Some(left), Some(p), Some(right)) => layoutForEnclosedChild(t, left, right) \\ (_ => trace("enclosed child"))
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
    trace("child leading:   %s", leadingLayoutFromChild.toString)
    trace("child trailing:  %s", trailingLayoutFromChild.toString)
    trace("parent trailing: %s", trailingLayoutFromParent.toString)
    
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
    
  def layoutForLeftOuterChild(t: Tree, p: Tree, right: Tree): (Layout, Layout) = 
    splitLayoutBetweenParentAndFirstChild(child = t, parent = p)._2 → 
    splitLayoutBetweenSiblings(left = t, right = right)._1 
    
  def layoutForRightOuterChild(t: Tree, p: Tree, left: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(left = left, right = t)._2  → 
    splitLayoutBetweenLastChildAndParent(child = t, parent = p)._1
    
  def layoutForEnclosedChild(t: Tree, left: Tree, right: Tree): (Layout, Layout) = 
    splitLayoutBetweenSiblings(left = left, right = t)._2 →
    splitLayoutBetweenSiblings(left = t, right = right)._1

  def splitLayoutBetweenParentAndFirstChild(child: Tree, parent: Tree): (Layout, Layout) = {
    
    implicit val currentFile = child.pos.source
    
    (parent, child) match {
      
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
         layout(p.pos.start, c.pos.start) splitAfter '{'
         
      case (p: Import, c) =>
         layout(p.pos.start, p.pos.point) → NoLayout
         
      case (p: ImportSelectorTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: SuperConstructorCall, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Ident, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Literal, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: SelfTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: TypeApply, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Function, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: TypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: AppliedTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: TypeBoundsTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Return, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: New, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Match, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: CaseDef, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Bind, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Typed, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Alternative, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: UnApply, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Star, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Try, c: Block) =>
         layout(p.pos.start, c.pos.start) splitBefore ('{')
         
      case (p: Try, c) =>
         layout(p.pos.start, c.pos.start) splitAfter ('\n', '{')
         
      case (p: Assign, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Throw, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: Annotated, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: LabelDef, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: ExistentialTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: SelectFromTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: SingletonTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: CompoundTypeTree, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
         
      case (p: MultipleAssignment, c) =>
         layout(p.pos.start, c.pos.start) → NoLayout
      
      case (p, t) => throw new Exception("Unhandled parent: "+ p.getClass.getSimpleName +", child: "+ t.getClass.getSimpleName)
    }
  }

   def splitLayoutBetweenLastChildAndParent(child: Tree, parent: Tree): (Layout, Layout) = {
     
     trace("splitting layout between child %s and parent %s", child.getClass.getSimpleName, parent.getClass.getSimpleName)
     
     implicit val currentFile = child.pos.source
     
     (child, parent) match {
       
       case (c: Block, p) =>
         layout(c.pos.end, p.pos.end) splitAfter '}'
       
       case (c, p: PackageDef) =>
         layout(c.pos.end, p.pos.end) splitAfter '\n'
         
       case (c, p @ (_: ClassDef | _: ModuleDef)) =>
         layout(c.pos.end, p.pos.end) splitAfter '}'
         
       case (c, p: Template) =>
         layout(c.pos.end, p.pos.end) splitBefore (')', '\n')
         
       case (c, p: If) =>
         layout(c.pos.end, p.pos.end) splitBefore (')')
         
       // values's right hand side can be wrapped in { } without being a block:
       case (c, p: ValDef) if c.sameTree(p.rhs) && c.originalLeftSibling.isDefined =>
         
         val left = c.originalLeftSibling.get
         
         if(layout(left.pos.end, c.pos.start).contains("{")) {
           
           val nextPos = (p.originalRightSibling.map(_.pos.start) match {
             case None => p.originalParent map (_.pos.end - 1) // exclude the parent's trailing }
             case some => some
           }) getOrElse p.pos.end
           
           if(layout(c.pos.end, nextPos).contains("}")) {
             layout(c.pos.end, nextPos) splitAfter '}'
           } else {
             NoLayout → layout(c.pos.end, p.pos.end)
           }
           
         } else {
           NoLayout → layout(c.pos.end, p.pos.end)
         }
     
       case (c, p: ValOrDefDef) =>
         layout(c.pos.end, p.pos.end) splitAfter '}'
         
       case (c, p: Apply) =>
         NoLayout → layout(c.pos.end, p.pos.end)
       
       case (c, p: Select) =>
         NoLayout → NoLayout
        
       case (c, p: Block) =>
         layout(c.pos.end, p.pos.end) splitAfter '\n'
         
       case (c, p: Import) =>
         NoLayout → layout(c.pos.end, p.pos.end) // for renames, associate the '}' to the parent
         
       case (c, p: ImportSelectorTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: SuperConstructorCall) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Ident) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Literal) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: SelfTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: TypeApply) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Function) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: TypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: AppliedTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: TypeBoundsTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Return) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: New) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: TypeDef) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Match) =>
         layout(c.pos.end, p.pos.end) splitBefore ('\n')
         
       case (c, p: CaseDef) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Bind) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Typed) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Alternative) =>
         NoLayout → layout(c.pos.end, p.pos.end)
       
       case (c, p: UnApply) =>
         NoLayout → layout(c.pos.end, p.pos.end)
       
       case (c, p: Star) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Try) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Assign) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Throw) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: Annotated) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: LabelDef) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: ExistentialTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: SelectFromTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: SingletonTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: CompoundTypeTree) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p: MultipleAssignment) =>
         NoLayout → layout(c.pos.end, p.pos.end)
         
       case (c, p) => throw new Exception("Unhandled parent: "+ p.getClass.getSimpleName +", child: "+ c.getClass.getSimpleName)
     }
   }
   
  private val Else = """(?ms)(.*?)(?:\n\s*\}\s*)?\n?\s*else\s*(?:[\s\{]*\n\s*)?(.*)""".r
  private val StartComment = """(.*?)(/\*.*)""".r
  private val Class = """(.*?)(class.*)""".r
  private val EmptyParens = """(?ms)(.*?\(\s*\)\s*)(.*)""".r
  private val OpeningBrace = """(.*?)\((.*)""".r
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
 
  def splitLayoutBetweenSiblings(left: Tree, right: Tree): (Layout, Layout) = {
      
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
    
    (left, right) match {
      case (_, EmptyTree) | (EmptyTree, _) => NoLayout → NoLayout
      case (l: Import, r: Import) => NoLayout → NoLayout
        
      case (l, r: ImportSelectorTree) =>
        // All the layout, like '.' and '{' belongs to the selector.
        layout(l.pos.end, r.pos.start)(l.pos.source) → NoLayout

      case (l, r) => 
        
        val source = between(l, r)(left.pos.source).toString
        val (layout, comments) = splitComment(source)
        
        val (ll: String, lr: String, rule: String) = (l, r) match {
            
          case (l: ValOrDefDef, r: ValOrDefDef) => 
            layout match {
              case Comma(l, r)   => (l, r, "Comma")
              case NewLine(l, r) => (l, r, "NewLine")
              case _ => split(layout)
            }
            
          case (l, r) => split(layout)
        }
        
        trace("Rule %s splits (%s, %s) layout %s into %s and %s", rule, l.getClass.getSimpleName, r.getClass.getSimpleName, layout, ll, lr)
        Layout(mergeLayoutWithComment(ll, comments)) → Layout(mergeLayoutWithComment(lr reverse, comments reverse) reverse)
    }
  }
}