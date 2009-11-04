package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer

object Space {
  
  def space(p1: Position, p2: Position) = (p1, p2) match {
    case _ if p2 precedes p1 => nullPart
    case _ if p1.end < p2.start => new WhiteSpacePart(p1.end, p2.start, p1.source)
    case _ if p1.start < p2.start => new WhiteSpacePart(p1.start, p2.start, p1.source)
    case _ if p1 == p2 => new WhiteSpacePart(p1.start, p1.point, p1.source)
    case _ if (p2 includes p1) &&  (p2.start < p1.start) => new WhiteSpacePart(p1.end, p2.end, p1.source)
    case _ if p1.end == p2.end => new WhiteSpacePart(p1.start, p2.start, p1.source)
    case _ if p1.start == p2.start => new WhiteSpacePart(p1.end, p2.point, p1.source)
    case _ => new WhiteSpacePart(p1.start, p2.start, p1.source)
  }

  def space(t: Trees#Tree) = new WhiteSpacePart(t.pos.start, t.pos.point, t.pos.source)
  def space(t: Trees#Tree, l: List[Trees#Tree]): WhiteSpacePart = if (l.isEmpty) nullPart else space(t, l.head)
  def space(l1: List[Trees#Tree], l2: List[Trees#Tree]): WhiteSpacePart = if (l2.isEmpty || l1.isEmpty) nullPart else space(l1.last, l2.head)
  def space(t1: Trees#Tree, t2: Trees#Tree): WhiteSpacePart = space(t1.pos, t2.pos)
  def space(p: Position, t: Trees#Tree): WhiteSpacePart = space(p, t.pos)
  def space(t: Trees#Tree, p: Position): WhiteSpacePart = space(t.pos, p)
}

object Partitioner {

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

  def apply(trees: Trees, root: Trees#Tree) : List[Part] = {
    
      import trees._
      type Tree = Trees#Tree
      
      val s = new ListBuffer[Part]
                             
      def add(se: Part) { 
        if(s != nullPart && se.toString != "") {
          s += se
        }
      }

      object visitors {
        
        def withRange(t: Tree) = t.pos.isRange
                                                    
        def empty(ts: List[Tree]) = !ts.exists(withRange)
        
        def afterName(t: DefTree) = new RangePosition(t.pos.source, t.pos.point + t.name.toString.length, t.pos.point + t.name.toString.length, t.pos.end)
        
        def visitAll(trees: List[Tree]): Unit = iterateInPairs(trees filter withRange)(visit(_))((t1: Tree, t2: Tree) => add(space(t1,t2)))
        
        def visit(tree: Tree): Unit = {
           
          def modifiers(mods: Modifiers) = iterateInPairs(mods.positions) {
            (x: Pair[Long, Position]) => add(new FlagPart(x._1, x._2))
          }{
            (x: Pair[Long, Position], y: Pair[Long, Position]) => add(space(x._2, y._2))
          }
          
          def classOrObject(pos: Position, mods: Modifiers) {
            modifiers(mods)
           if (mods.positions.isEmpty)
              add(space(pos, pos))
            else
              add(new WhiteSpacePart(mods.positions.last._2.end + 1, pos.point, pos.source))
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
            add(new SymbolPart(c))
            //s += space(afterName(c), impl)
            add(new WhiteSpacePart(c.pos.point + name.length, impl.pos.start, c.pos.source))
            visit(impl)
            
          case m @ ModuleDef(mods, name, impl) => 
            classOrObject(m.pos, mods)
            add(new SymbolPart(m))
            
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
                add(new WhiteSpacePart(trueBody.last.pos.end, t.pos.end, t.pos.source))
            }
            
          case v @ ValDef(mods, name, typ, rhs) => 
            modifiers(mods)
            if(mods.positions.isEmpty) {
              add(space(v, v.symbol.pos))
            } else {
              add(space(mods.positions.last._2, v.symbol.pos) offset 1)
            }
            add(new SymbolPart(v))
            add(space(v.symbol.pos, if(withRange(typ)) typ else rhs) offset (v.symbol.nameString.length + v.symbol.pos.point - v.symbol.pos.start)) // FIXME name is too long
            visit(typ)
            //s += space(typ, rhs)
            visit(rhs)
            
          case t: TypeTree => if(t.original != null) visit(t.original)

          case i: Ident =>
            add(new SymbolPart(i))
                      
          case select @ Select(qualifier, name) if qualifier.symbol.pos == NoPosition =>
            if(withRange(select))
              add(new SymbolPart(select))
            
          case select @ Select(qualifier, name)  =>
            visit(qualifier)
            add(space(qualifier, select))
            add(new SymbolPart(select))
            
          case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            modifiers(mods)
            add(space(defdef))
            add(new SymbolPart(defdef))

            add(space(afterName(defdef), tpt))
            visit(tpt)
            visit(rhs)

          case x => ;//println("Unknown Tree: "+ x)
          
          }
        }
      }
      
      add(new WhiteSpacePart(0, root.pos.start, root.pos.source))
      visitors.visit(root)
      if(root.pos.end != root.pos.source.length - 1)
        add(new WhiteSpacePart(root.pos.end, root.pos.source.length, root.pos.source))
      
      s.toList
  }
}
