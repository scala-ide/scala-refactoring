package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer


object Printer {
  
  trait SourceElement {
    def print(out: Appendable)
    override def toString = {
      val sb = new java.lang.StringBuilder
      print(sb)
      sb.toString
    }
  }
  
  object nullSourceElement extends FromFileSourceElement(0, 0, null) {
    override def print(out: Appendable) = ()
  }
  
  class FromFileSourceElement(start: Int, end: Int, file: SourceFile) extends SourceElement {
    def print(out: Appendable) {
      file.content.slice(start, end).foreach(out append _)
    }
    
    def offset(o: Int) = new FromFileSourceElement(start + o, end, file)
    def offset(s: String) = new FromFileSourceElement(start + s.length, end, file)
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
      case _            => "<unknown>: " + flagsToString(flag)
    })
  }

  def between(start: Int, end: Int, file: SourceFile) = new FromFileSourceElement(start, end, file)

  def space(p1: Position, p2: Position) = (p1, p2) match {
    case _ if p2 precedes p1 => nullSourceElement
    case _ if p1.end < p2.start => between(p1.end, p2.start, p1.source)
    case _ if p1.start < p2.start => between(p1.start, p2.start, p1.source)
    case _ if p1 == p2 => between(p1.start, p1.point, p1.source)
    case _ if (p2 includes p1) &&  (p2.start < p1.start) => new FromFileSourceElement(p1.end, p2.end, p1.source)
    case _ if p1.end == p2.end => new FromFileSourceElement(p1.start, p2.start, p1.source)
    case _ if p1.start == p2.start => new FromFileSourceElement(p1.end, p2.point, p1.source)
    case _ => new FromFileSourceElement(p1.start, p2.start, p1.source)
  }

  def space(t: Trees#Tree) = between(t.pos.start, t.pos.point, t.pos.source)
  def space(t: Trees#Tree, l: List[Trees#Tree]): FromFileSourceElement = if (l.isEmpty) nullSourceElement else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): FromFileSourceElement = if (l2.isEmpty || l1.isEmpty) nullSourceElement else space(l1.last, l2.head)
  def space(t1: Trees#Tree, t2: Trees#Tree): FromFileSourceElement = space(t1.pos, t2.pos)
  def space(p: Position, t: Trees#Tree): FromFileSourceElement = space(p, t.pos)
  def space(t: Trees#Tree, p: Position): FromFileSourceElement = space(t.pos, p)
  
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

        def withRange(t: Tree) = t.pos.isInstanceOf[RangePosition]
                                                    
        def empty(ts: List[Tree]) = !ts.exists(withRange)
        
        def visitAll(trees: List[Tree]): Unit = iterateInPairs(trees filter withRange)(visit(_))(s += space(_,_))
        
        def visit(tree: Tree): Unit = {
          
          def modifiers(mods: Modifiers) = iterateInPairs(mods.positions) {
            (x: Tuple2[Long, Position]) => s += new FlagSourceElement(x._1); println(s.last)
          }{
            (x: Tuple2[Long, Position], y: Tuple2[Long, Position]) => s += space(x._2, y._2); println(s.last)
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
            
            val (classParams, restBody) = body.partition {
              case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
              case _ => false
            }
            
            val(trueBody, earlyBody) = restBody.filter(withRange).partition( (t: Tree) => parents.forall(_.pos precedes t.pos))
            
            val trueParents = parents filter withRange
            
            s += space(t, classParams); println(s.last)
            
            if(classParams.isEmpty) {
              s += space(t, trueParents); println(s.last)
            } else {
	            visitAll(classParams)
              s += space(classParams, trueParents); println(s.last)
            }
            
            if(empty(trueParents) && empty(trueBody) && !empty(classParams)) {
              s += space(classParams.last, t.pos); println(s.last)
            }
            
            visitAll(trueParents)
            s += space(trueParents, trueBody); println(s.last)
            visitAll(trueBody)
            
            // {} with an empty body
            if(trueBody.isEmpty && !trueParents.isEmpty) {
              val maxPos = trueParents.map(_.pos).reduceLeft((p1: Position, p2: Position) => if(p1.end > p2.end) p1 else p2)
              s += space(trueParents.last, t.pos)
            } else {
              if(!trueBody.isEmpty)
                s += between(trueBody.last.pos.end, t.pos.end, t.pos.source)
            }
            
          case v @ ValDef(mods, name, typ, rhs) => 
            modifiers(mods)
            if(mods.positions.isEmpty) {
              s += space(v, v.symbol.pos); println(s.last)
            } else {
              s += space(mods.positions.last._2, v.symbol.pos) offset 1; println(s.last)
            }
            s += new StringSourceElement(name.toString.trim); println(s.last)
            s += space(v.symbol.pos, if(withRange(typ)) typ else rhs) offset (name.length - 1 + v.symbol.pos.point - v.symbol.pos.start); println(s.last) // FIXME name is too long
            visit(typ)
            
          case t: TypeTree => if(t.original != null) visit(t.original)
            
          case i @ Ident(sym) => 
            if (i.symbol.pos != NoPosition) {
              s += new StringSourceElement(sym.toString); println(s.last)
            } else {
              s += new StringSourceElement(i.name.toString); println(s.last)
            }
                      
          case select @ Select(qualifier, name) if qualifier.symbol.pos == NoPosition =>
            if(select.pos.isInstanceOf[RangePosition])
              s += new StringSourceElement(name.toString)
            
          case select @ Select(qualifier, name)  =>
            visit(qualifier)
            s += space(qualifier, select)
            s += new StringSourceElement(name.toString)
            
          case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            modifiers(mods)
            s += space(defdef); println(s.last)
            s += new StringSourceElement(name.toString); println(s.last)
            
            def afterName(t: DefTree) = new RangePosition(t.pos.source, t.pos.point + t.name.toString.length, t.pos.point + t.name.toString.length, t.pos.end) 
            
            s += space(afterName(defdef), tpt); println(s.last)
            visit(tpt)
            visit(rhs)
            
  
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
