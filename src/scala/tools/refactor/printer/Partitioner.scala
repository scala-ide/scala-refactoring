package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer

trait Partitioner {
  self: scala.tools.refactor.Compiler =>
  import compiler._

  import WhiteSpaceBetweenTrees._

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

  def splitIntoParts(root: Tree) : List[Part] = {
    
    val s = new ListBuffer[OriginalSourcePart]
    
    s += BeginOfFile(root.pos.source)
                           
    def add(se: OriginalSourcePart) = se match {
      case NullPart => ()
      case se if se.print == "" => ()
      case _ => s += se
    }

    def withRange(t: Tree) = t.pos.isRange
                                                
    def empty(ts: List[Tree]) = !ts.exists(withRange)
    
    def afterName(t: DefTree) = new RangePosition(t.pos.source, t.pos.point + t.name.toString.length, t.pos.point + t.name.toString.length, t.pos.end)
    
    def visitAll(trees: List[Tree])(after: (Part) => Unit ): Unit = iterateInPairs(trees filter withRange) {
        (tree) => visit(tree)
      }{
        (t1, t2) => 
        after(s.last)
        add(space(t1,t2))
      }
    
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
          add(new WhitespacePart(mods.positions.last._2.end + 1, pos.point, pos.source))
      }
      
      tree match {
      
      case p @ PackageDef(pid, stats) if pid.symbol.pos == NoPosition =>
        add(space(p))
        visitAll(stats)(part => ())

      case p @ PackageDef(pid, stats) =>  
        add(space(p, pid))
        visit(pid)
        add(space(pid, stats))
        visitAll(stats)(part => ())
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        classOrObject(c.pos, mods)
        add(new SymTreePart(c))
        //s += space(afterName(c), impl)
        add(new WhitespacePart(c.pos.point + name.length, impl.pos.start, c.pos.source))
        visit(impl)
        
      case m @ ModuleDef(mods, name, impl) => 
        classOrObject(m.pos, mods)
        add(new SymTreePart(m))
        
      case t @ Template(parents, _, body) =>
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        val(trueBody, earlyBody) = restBody.filter(withRange).partition( (t: Tree) => parents.forall(_.pos precedes t.pos))
        
        val trueParents = parents filter withRange

        add(space(t, classParams))
                
        visitAll(classParams) {
          case part: WithRequirement => part.requirePost(", ")
          case _ => ()
        }
        
        if(classParams.isEmpty) {
          add(space(t, trueParents))
        } else {
          add(space(classParams, trueParents))
        }
        
        if(trueParents.isEmpty && trueBody.isEmpty && !empty(classParams)) {
          add(space(classParams.last, t.pos))
        }
        
        visitAll(trueParents)(part => ())
        
        add(space(if(!trueParents.isEmpty) trueParents.last else t, trueBody))
        
        visitAll(trueBody)(part => ())
        
        // {} with an empty body
        if(trueBody.isEmpty && !trueParents.isEmpty) {
          add(space(trueParents.last, t.pos))
        } else {
          if(!trueBody.isEmpty)
            add(new WhitespacePart(trueBody.last.pos.end, t.pos.end, t.pos.source))
        }
        
      case v @ ValDef(mods, name, typ, rhs) => 
        modifiers(mods)
        if(mods.positions.isEmpty) {
          add(space(v, v.symbol.pos))
        } else {
          
          add(space(mods.positions.last._2, v.symbol.pos) offset 1)
        }
        add(new SymTreePart(v))
        add(space(v.symbol.pos, if(withRange(typ)) typ else rhs) offset (v.symbol.nameString.length + v.symbol.pos.point - v.symbol.pos.start)) // FIXME name is too long
        visit(typ)
        //s += space(typ, rhs)
        visit(rhs)
        
      case t: TypeTree => if(t.original != null) visit(t.original)

      case i: Ident =>
        add(new SymTreePart(i))
                  
      case select @ Select(qualifier, name) if qualifier.symbol.pos == NoPosition =>
        if(withRange(select))
          add(new SymTreePart(select))
        
      case select @ Select(qualifier, name)  =>
        visit(qualifier)
        add(space(qualifier, select))
        add(new SymTreePart(select))
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        modifiers(mods)
        add(space(defdef))
        add(new SymTreePart(defdef))

        add(space(afterName(defdef), tpt))
        visit(tpt)
        visit(rhs)

      case x => ;//println("Unknown Tree: "+ x)
      
      }
    }
    
    add(new WhitespacePart(0, root.pos.start, root.pos.source))
    visit(root)
    add(new WhitespacePart(s.last.end, root.pos.source.length, root.pos.source))
    
    s += EndOfFile(root.pos.source)
    
    s.toList
  }
}
