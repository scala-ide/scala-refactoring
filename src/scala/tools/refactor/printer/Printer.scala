package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer

trait SourceElement {
  def print(out: Appendable)
  override def toString = {
    val sb = new java.lang.StringBuilder
    print(sb)
    sb.toString
  }
}

object nullSourceElement extends WhiteSpaceSourceElement(0, 0, null) {
  override def print(out: Appendable) = ()
}

class WhiteSpaceSourceElement(start: Int, end: Int, file: SourceFile) extends SourceElement {
  def print(out: Appendable) {
    file.content.slice(start, end).foreach(out append _)
  }
  
  def offset(o: Int) = new WhiteSpaceSourceElement(start + o, end, file)
}

class SymbolTreeElement(tree: Trees#SymTree) extends SourceElement {
  def print(out: Appendable) {
    val sym = tree.symbol
    val src = tree.pos.source.asInstanceOf[BatchSourceFile]
    src.content.slice(tree.pos.point, tree.pos.point + sym.nameString.length).foreach(out append _)
  }
}

class FlagSourceElement(flag: Long, pos: Position) extends SourceElement {
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
    case Tokens.VAL   => "val"
    case _            => "<unknown>: " + flagsToString(flag)
  })
}
  
object Space {
  
  def space(p1: Position, p2: Position) = (p1, p2) match {
    case _ if p2 precedes p1 => nullSourceElement
    case _ if p1.end < p2.start => new WhiteSpaceSourceElement(p1.end, p2.start, p1.source)
    case _ if p1.start < p2.start => new WhiteSpaceSourceElement(p1.start, p2.start, p1.source)
    case _ if p1 == p2 => new WhiteSpaceSourceElement(p1.start, p1.point, p1.source)
    case _ if (p2 includes p1) &&  (p2.start < p1.start) => new WhiteSpaceSourceElement(p1.end, p2.end, p1.source)
    case _ if p1.end == p2.end => new WhiteSpaceSourceElement(p1.start, p2.start, p1.source)
    case _ if p1.start == p2.start => new WhiteSpaceSourceElement(p1.end, p2.point, p1.source)
    case _ => new WhiteSpaceSourceElement(p1.start, p2.start, p1.source)
  }

  def space(t: Trees#Tree) = new WhiteSpaceSourceElement(t.pos.start, t.pos.point, t.pos.source)
  def space(t: Trees#Tree, l: List[Trees#Tree]): WhiteSpaceSourceElement = if (l.isEmpty) nullSourceElement else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): WhiteSpaceSourceElement = if (l2.isEmpty || l1.isEmpty) nullSourceElement else space(l1.last, l2.head)
  def space(t1: Trees#Tree, t2: Trees#Tree): WhiteSpaceSourceElement = space(t1.pos, t2.pos)
  def space(p: Position, t: Trees#Tree): WhiteSpaceSourceElement = space(p, t.pos)
  def space(t: Trees#Tree, p: Position): WhiteSpaceSourceElement = space(t.pos, p)
}

object Printer {

  import Space._

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
      type Tree = Trees#Tree
      
      val s = new ListBuffer[SourceElement]
                             
      def add(se: SourceElement) { 
        if(s != nullSourceElement && se.toString != "") {
          s += se
          se match {
            case se: WhiteSpaceSourceElement => println("Space  ["+se+"]")
            case se: SymbolTreeElement =>       println("Symbol ["+se+"]")
            case se: FlagSourceElement =>       println("Flag   ["+se+"]")
          }
        }
      }

      object visitors {
        
        def withRange(t: Tree) = t.pos.isRange
                                                    
        def empty(ts: List[Tree]) = !ts.exists(withRange)
        
        def afterName(t: DefTree) = new RangePosition(t.pos.source, t.pos.point + t.name.toString.length, t.pos.point + t.name.toString.length, t.pos.end)
        
        def visitAll(trees: List[Tree]): Unit = iterateInPairs(trees filter withRange)(visit(_))((t1: Tree, t2: Tree) => add(space(t1,t2)))
        
        def visit(tree: Tree): Unit = {
           
          def modifiers(mods: Modifiers) = iterateInPairs(mods.positions) {
            (x: Pair[Long, Position]) => add(new FlagSourceElement(x._1, x._2))
          }{
            (x: Pair[Long, Position], y: Pair[Long, Position]) => add(space(x._2, y._2))
          }
          
          def classOrObject(pos: Position, mods: Modifiers) {
            modifiers(mods)
           if (mods.positions.isEmpty)
              add(space(pos, pos))
            else
              add(new WhiteSpaceSourceElement(mods.positions.last._2.end + 1, pos.point, pos.source))
          }
          
          tree match {
          
          case p @ PackageDef(pid, stats) if pid.symbol.pos == NoPosition =>
            add(space(p))
            visitAll(stats)

          case p @ PackageDef(pid, stats) =>  
            add(space(p, pid))
            visit(pid)
            add(space(pid, stats))
            visitAll(stats)
            
          case c @ ClassDef(mods, name, tparams, impl) =>
            classOrObject(c.pos, mods)
            add(new SymbolTreeElement(c))
            //s += space(afterName(c), impl)
            add(new WhiteSpaceSourceElement(c.pos.point + name.length, impl.pos.start, c.pos.source))
            visit(impl)
            
          case m @ ModuleDef(mods, name, impl) => 
            classOrObject(m.pos, mods)
            add(new SymbolTreeElement(m))
            
          case t @ Template(parents, _, body) =>          
            
            val (classParams, restBody) = body.partition {
              case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
              case _ => false
            }
            
            val(trueBody, earlyBody) = restBody.filter(withRange).partition( (t: Tree) => parents.forall(_.pos precedes t.pos))
            
            val trueParents = parents filter withRange

            add(space(t, classParams))
            
            visitAll(classParams)
            
            if(classParams.isEmpty) {
              add(space(t, trueParents))
            } else {
              add(space(classParams, trueParents))
            }
            
            if(trueParents.isEmpty && trueBody.isEmpty && !empty(classParams)) {
              add(space(classParams.last, t.pos))
            }
            
            visitAll(trueParents)
            
            add(space(if(!trueParents.isEmpty) trueParents.last else t, trueBody))
            
            visitAll(trueBody)
            
            // {} with an empty body
            if(trueBody.isEmpty && !trueParents.isEmpty) {
              add(space(trueParents.last, t.pos))
            } else {
              if(!trueBody.isEmpty)
                add(new WhiteSpaceSourceElement(trueBody.last.pos.end, t.pos.end, t.pos.source))
            }
            
          case v @ ValDef(mods, name, typ, rhs) => 
            modifiers(mods)
            if(mods.positions.isEmpty) {
              add(space(v, v.symbol.pos))
            } else {
              add(space(mods.positions.last._2, v.symbol.pos) offset 1)
            }
            add(new SymbolTreeElement(v))
            add(space(v.symbol.pos, if(withRange(typ)) typ else rhs) offset (v.symbol.nameString.length + v.symbol.pos.point - v.symbol.pos.start)) // FIXME name is too long
            visit(typ)
            //s += space(typ, rhs)
            visit(rhs)
            
          case t: TypeTree => if(t.original != null) visit(t.original)

          case i: Ident =>
            add(new SymbolTreeElement(i))
                      
          case select @ Select(qualifier, name) if qualifier.symbol.pos == NoPosition =>
            if(withRange(select))
              add(new SymbolTreeElement(select))
            
          case select @ Select(qualifier, name)  =>
            visit(qualifier)
            add(space(qualifier, select))
            add(new SymbolTreeElement(select))
            
          case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            modifiers(mods)
            add(space(defdef))
            add(new SymbolTreeElement(defdef))

            add(space(afterName(defdef), tpt))
            visit(tpt)
            visit(rhs)

          case x => ;//println("Unknown Tree: "+ x)
          
          }
        }
      }
      
      add(new WhiteSpaceSourceElement(0, root.pos.start, root.pos.source))
      visitors.visit(root)
      if(root.pos.end != root.pos.source.length - 1)
        add(new WhiteSpaceSourceElement(root.pos.end, root.pos.source.length, root.pos.source))
        
      println("==============")
      
      s foreach (_.print(out))
  }
}
