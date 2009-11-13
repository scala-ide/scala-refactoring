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
    
    def scope(t: Tree)(body: => Unit) = {
      val newScope = CompositePart(t)
      scopes.top add newScope
      scopes push newScope
      body
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
        scope(tree) {
          mods.positions foreach addModifiers
          scopes.top add new SymTreePart(m)
          super.traverse(tree)
        }
        
      case v @ ValDef(mods, name, typ, rhs) => 
        scope(tree) {
          mods.positions foreach addModifiers
          scopes.top add new SymTreePart(v)
          super.traverse(tree)
        }

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
            //FIXME separator(collector.last)
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
          scope(tree) { // fix the start 
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
      
      val rootPart = CompositePart(tree)
      
      //rootPart add BeginOfFile(tree.pos.source)
      
      scopes push rootPart
      
      traverse(tree)
      
      //rootPart add EndOfFile(tree.pos.source)
      
      rootPart
    }
  }
  
  def essentialParts(root: Tree) = new Visitor().visit(root)
  
  def splitIntoParts(root: Tree): CompositePart = {
    
    val parts = essentialParts(root)
    
    def fillWs(part: Part): Part = part match {
      case p: CompositePart => 
        val newCp = CompositePart(p.tree)
        
        val childrenPairs = p.children zip p.children.tail
        
        //println("pairs of children: "+ childrenPairs)

        (childrenPairs) foreach {
          case (left: CompositePart#BeginOfScope, right: OriginalSourcePart) => ()
            newCp add WhitespacePart(left.end, right.start, left.file)
          case (left: OriginalSourcePart, right: OriginalSourcePart) =>
            newCp add (fillWs(left))
            newCp add WhitespacePart(left.end, right.start, left.file)
        }
        newCp
      case _ => part
    }
    
    fillWs(parts).asInstanceOf[CompositePart]
    
    /*
    var currentBraceCount = 0
    
    def createWhitespaceParts(start: Int, end: Int, file: SourceFile): List[Part] = {
      
      // should ignore comments
      
      val ws = new String(file.content.slice(start, end))
      
      val ws2 = ws.foldRight(List[StringBuilder](new StringBuilder)) {
        (c: Char, l: List[StringBuilder]) => c match {
          case c if c == '{' => new StringBuilder :: (new StringBuilder("{")) :: l 
          case c if c == '}' => new StringBuilder :: (new StringBuilder("}")) :: l
          case c => l.first.insert(0, c); l
        }
      }
      
      val ws3 = ws2.map(_.toString).map {
        case c if c == "{" =>
          currentBraceCount += 1
          OpeningBracePart("{", currentBraceCount)
        case c if c == "}" =>
          currentBraceCount -= 1
          ClosingBracePart("}", currentBraceCount + 1)
        case ws => StringWhitespacePart(ws)
      }
      
//      println(ws2 map (_.toString) mkString "|")
      
      ws3
    }
        
    def whitespaceBetween(p: Part, ps: List[Part]): List[Part] = p :: ps match {
      case (eof: EndOfFile) :: Nil => eof :: Nil
      case (p1: OriginalSourcePart) :: (p2: OriginalSourcePart) :: _ => 
        if(p1.end < p2.start) {
          p :: createWhitespaceParts(p1.end, p2.start, p1.file) ::: ps
        } else
          p :: ps
    }
    
    essentialParts(root).foldRight(List[Part]())(whitespaceBetween)*/
  }
}
