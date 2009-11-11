package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer

trait Partitioner {
  self: scala.tools.refactor.Compiler =>
  import compiler._
  
  private class Visitor extends Traverser {
    
    type PartList = ListBuffer[OriginalSourcePart]
    
    private var collector: PartList = _
    
    val addModifiers = (x: Pair[Long, Position]) => collector += new FlagPart(x._1, x._2)
    
    override def traverse(tree: Tree) = tree match {
      
      case tree if !tree.pos.isRange => ()
      
      case t: TypeTree => if(t.original != null) traverse(t.original)
      
      case i: Ident =>
        if (i.symbol.pos == NoPosition)
          collector += new SymTreePart(i) {
            override val end = start + i.name.length
        }
        else
          collector += new SymTreePart(i)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        mods.positions foreach addModifiers
        collector += new SymTreePart(c)
        super.traverse(c)
        
      case m @ ModuleDef(mods, name, impl) => 
        mods.positions foreach addModifiers
        collector += new SymTreePart(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, typ, rhs) => 
        mods.positions foreach addModifiers
        collector += new SymTreePart(v)
        super.traverse(tree)

      case select @ Select(qualifier, name)  =>
        traverse(qualifier)
        
        // An ugly hack, sorry
        if (qualifier.pos.isRange) {
          collector += new SymTreePart(select) {
            override val start = select.pos.end - select.symbol.nameString.length
            override val end = select.pos.end
          }
        } else {
          collector += new SymTreePart(select)
        }
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        mods.positions foreach addModifiers
        if(defdef.pos.point >=defdef.pos.start)
          collector += new SymTreePart(defdef)
        super.traverse(tree)
        
     case typeDef @ TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =>
        mods.positions foreach addModifiers
        collector += new SymTreePart(typeDef)
        super.traverse(tree)
        /*
      case pkg @ PackageDef(pid, stats) if pkg.symbol.pos == NoPosition =>
        super.traverse(tree)
        
      case pkg @ PackageDef(pid, stats) =>
        collector += new SymTreePart(pkg)
        super.traverse(tree)*/
        
      case t @ Template(parents, _, body) =>
        
        def visitAll(trees: List[Tree])(separator: (Part) => Unit ): Unit = trees match {
          case Nil => 
            ()
          case x :: Nil => 
            traverse(x)
          case x :: xs => 
            traverse(x)
            separator(collector.last)
            visitAll(xs)(separator)
        }
        
        def withRange(t: Tree) = t.pos.isRange
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        visitAll(classParams) {
          case part: WithRequirement => part.requirePost(", ")
          case _ => ()
        }
        
        val(trueBody, earlyBody) = restBody.filter(withRange).partition( (t: Tree) => parents.forall(_.pos precedes t.pos))

        visitAll(earlyBody)(part => ())
        
        visitAll(parents)(part => ())        
                
        visitAll(trueBody)(part => ())
        
      case apply @ Apply(fun, args) =>
        super.traverse(tree)
        
      case lit @ Literal(constant) =>
        collector += new LiteralPart(lit)
        super.traverse(tree)
        
      case _ =>
        println("Not handled: "+ tree.getClass())
        super.traverse(tree)
    }
    
    def visit(tree: Tree) = {
      collector = new PartList
      collector += BeginOfFile(tree.pos.source)
      traverse(tree)
      collector += EndOfFile(tree.pos.source)
      collector.toList
    }
  }
  
  def splitIntoParts(root: Tree) : List[Part] = {
    
    val essentialParts = new Visitor().visit(root)
    
    def whitespaceBetween(p: Part, ps: List[Part]): List[Part] = p :: ps match {
      case (eof: EndOfFile) :: Nil => eof :: Nil
      case (p1: OriginalSourcePart) :: (p2: OriginalSourcePart) :: _ => 
        if(p1.end < p2.start) {
          p :: WhitespacePart(p1.end, p2.start, p1.file) :: ps
        } else
          p :: ps
    }
    
    essentialParts.foldRight(List[Part]())(whitespaceBetween)
  }
}
