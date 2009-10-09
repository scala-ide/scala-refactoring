package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer


object Printer {
  
  trait SourceElement {
    def print(out: Appendable)
  }
  
  object nullSourceElement extends SourceElement {
    def print(out: Appendable) = ()
  }
  
  class FromFileSourceElement(start: Int, end: Int, file: SourceFile) extends SourceElement {
    def print(out: Appendable) {
      file.content.slice(start, end).foreach(out append _)
    }
  }
  
  class StringSourceElement(text: String) extends SourceElement {
    def print(out: Appendable) = out append text
  }
  
  class FlagSourceElement(flag: Long) extends SourceElement {
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
      case _            => "<unknown>"
    })
  }

  def between(start: Int, end: Int, file: SourceFile) = new FromFileSourceElement(start, end, file)

  def space(p1: Position, p2: Position) = (p1, p2) match {
    case _ if p2 precedes p1 => nullSourceElement
    case (pos, _) if p1 == p2 => between(pos.start, pos.point, pos.source)
    case (p1, p2) if (p2 includes p1) &&  (p2.start < p1.start) => new FromFileSourceElement(p1.end, p2.end, p1.source)
    case (p1, p2) => new FromFileSourceElement(p1.end, p2.start, p1.source)
  }
  def space(t: Trees#Tree) = between(t.pos.start, t.pos.point, t.pos.source)
  def space(t1: Trees#Tree, t2: Trees#Tree) = (t1.pos, t2.pos) match {
    case (p1, p2: RangePosition) if p1.end < p2.start => between(p1.end, p2.start, p1.source)
    case (p1, p2: RangePosition) if p1.start < p2.start => between(p1.start, p2.start, p1.source)
    case (p1, p2: RangePosition) => between(p1.end, p2.point, p1.source)
    case _ => nullSourceElement
  }
  def space(t: Trees#Tree, l: List[Trees#Tree]): SourceElement = if (l.isEmpty) nullSourceElement else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): SourceElement = if (l2.isEmpty) nullSourceElement else space(l1.last, l2.head)
  
  def iterateInPairs[T](l: Iterable[T])(f: T => Unit)(between: (T, T) => Unit): Unit = l match {
    case Nil => ()
    case x if x.isEmpty => ()
    case x => val last = x reduceLeft {
        (t1: T, t2: T) => 
        f(t1)
        between(t1, t2)
        t2
      }
      f(last)
  }

  def apply(out: Appendable, trees: Trees, root: Trees#Tree) : Unit = {
    
      import trees._
      
      val s = new ListBuffer[SourceElement]

      type Tree = Trees#Tree
      
      object visitors {
        
        def visitAll(trees: List[Tree]): Unit = iterateInPairs(trees.toList)(visit(_))(s += space(_, _))
        
        def visit(tree: Tree): Unit = {
          
          def modifiers(mods: Modifiers) = iterateInPairs(mods.positions) {
            (x: Tuple2[Long, Position]) => s += new FlagSourceElement(x._1)
          }{
            (x: Tuple2[Long, Position], y: Tuple2[Long, Position]) => s += space(x._2, y._2)
          }
          
          def classOrObject(pos: Position, mods: Modifiers, name: String) {
            modifiers(mods)
            s += (if (mods.positions.isEmpty)
              space(pos, pos)
            else
              new FromFileSourceElement(mods.positions.last._2.end + 1, pos.point, pos.source))
            s += new StringSourceElement(name)
          }
          
          tree match {
          
          case p @ PackageDef(pid, stats) =>
            if (pid.symbol.pos == NoPosition) 
              s += space(p)
            else {
              s += space(p, pid)
              visit(pid)
              s += space(pid, stats)
            }
            visitAll(stats)
            
          case c @ ClassDef(mods, name, tparams, impl) =>
            classOrObject(c.pos, mods, name.toString)
            s += new FromFileSourceElement(c.pos.point + name.length, impl.pos.start, c.pos.source)
            visit(impl)
            
          case m @ ModuleDef(mods, name, impl) => 
            classOrObject(m.pos, mods, name.toString)
            
          case t @ Template(parents, _, body) =>
            s += space(t, parents)
            visitAll(parents)
            s += space(parents, body)
            visitAll(body)
            
            // {} with an empty body
            if(!body.exists(_.pos.isInstanceOf[RangePosition]) && parents.exists(_.pos.isInstanceOf[RangePosition])) {
              val maxPos = parents.filter(_.pos.isInstanceOf[RangePosition]).map(_.pos).reduceLeft((p1: Position, p2: Position) => if(p1.end > p2.end) p1 else p2)
              s += space(maxPos, t.pos)
            }
            
          case t: TypeTree => visit(t.original)
            
          case i @ Ident(sym) if i.symbol.pos != NoPosition => 
            s += new StringSourceElement(sym.toString)
                      
          case select @ Select(qualifier, name) if qualifier.symbol.pos == NoPosition =>
            if(select.pos.isInstanceOf[RangePosition])
              s += new StringSourceElement(name.toString)
            
          case select @ Select(qualifier, name)  =>
            visit(qualifier)
            s += space(qualifier, select)
            s += new StringSourceElement(name.toString)
  
          case x => println(x)
          
          }
        }
      }
      
      s += between(0, root.pos.start, root.pos.source)
      visitors.visit(root)
      if(root.pos.end != root.pos.source.length - 1)
        s += between(root.pos.end, root.pos.source.length, root.pos.source)
      
      s foreach (_.print(out))
  }
}
