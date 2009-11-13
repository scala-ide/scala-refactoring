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
    
    private var scopes = new scala.collection.mutable.Stack[CompositePart]
    
    def scope(t: Tree)(body: CompositePart => Unit) = {
      val newScope = CompositePart(t)
      scopes.top add newScope
      scopes push newScope
      body(newScope)
      scopes pop
    }
        
    val addModifiers = (x: Pair[Long, Position]) => scopes.top add new FlagPart(x._1, x._2)
    
    override def traverse(tree: Tree) = tree match {
      
      case tree if !tree.pos.isRange => ()
      
      case t: TypeTree => if(t.original != null) traverse(t.original)
      
      case i: Ident =>
        if (i.symbol.pos == NoPosition)
          scopes.top add new SymTreePart(i) {
            override val end = start + i.name.length
        }
        else
          scopes.top add new SymTreePart(i)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        mods.positions foreach addModifiers
        scopes.top add new SymTreePart(c)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) => 
        mods.positions foreach addModifiers
        scopes.top add new SymTreePart(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, typ, rhs) => 
        mods.positions foreach addModifiers
        scopes.top add new SymTreePart(v)
        super.traverse(tree)

      case select @ Select(qualifier, name)  =>
        traverse(qualifier)
        
        // An ugly hack, sorry
        if (qualifier.pos.isRange) {
          scopes.top add new SymTreePart(select) {
            override val start = select.pos.end - select.symbol.nameString.length
            override val end = select.pos.end
          }
        } else {
          scopes.top add new SymTreePart(select)
        }
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          mods.positions foreach addModifiers
          if(defdef.pos.point >=defdef.pos.start)
            scopes.top add new SymTreePart(defdef)
          super.traverse(tree)
        
      case typeDef @ TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =>
          mods.positions foreach addModifiers
          scopes.top add new SymTreePart(typeDef)
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
            separator(scopes.top.trueChildren.last)
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
                
        if(trueBody.size > 0) {
          scope(tree) { scope =>// fix the start
          
            val realStart = (classParams ::: earlyBody ::: (parents filter (_.pos.isRange))).foldLeft(scope.start) ( _ max _.pos.end )
            // actually need to skip even more
            scope.offset = realStart - scope.start
            
            visitAll(trueBody)(part => ())
          }
        }
        
      case apply @ Apply(fun, args) =>
        super.traverse(tree)
        
      case lit @ Literal(constant) =>
        scopes.top add new LiteralPart(lit)
        super.traverse(tree)
        
      case _ =>
        //println("Not handled: "+ tree.getClass())
        super.traverse(tree)
    }
    
    def visit(tree: Tree) = {
      
      val rootPart = new CompositePart(tree) {
        override val start = 0
        override val end = file.length
      }
      
      scopes push rootPart
      
      traverse(tree)
      
      rootPart
    }
  }
  
  def essentialParts(root: Tree) = new Visitor().visit(root)
  
  def splitIntoParts(root: Tree): CompositePart = {
    
    val parts = essentialParts(root)
    
    def fillWs(part: Part): Part = part match {
      case p: CompositePart => 
        
        val childrenPairs = p.children zip p.children.tail
        
        p.trueChildren.clear
        
        //println("pairs of children: "+ childrenPairs)
        
        def whitespace(start: Int, end: Int, file: SourceFile) {
          if(start < end)
            p add WhitespacePart(start, end, file)
        }

        (childrenPairs) foreach {
          case (left: CompositePart#BeginOfScope, right: OriginalSourcePart) => ()
            whitespace(left.end, right.start, left.file)
          case (left: OriginalSourcePart, right: OriginalSourcePart) =>
            p add (fillWs(left))
            whitespace(left.end, right.start, left.file)
        }
        p
      case _ => part
    }
    
    fillWs(parts).asInstanceOf[CompositePart]
  }
}
