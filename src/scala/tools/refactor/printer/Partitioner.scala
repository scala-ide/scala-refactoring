package scala.tools.refactor.printer

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer
import scala.tools.refactor.UnknownPosition

trait Partitioner {
  self: scala.tools.refactor.Compiler =>
  import compiler._
  
  private class Visitor extends Traverser {
    
    type PartList = ListBuffer[OriginalSourcePart]
    
    private var scopes = new scala.collection.mutable.Stack[CompositePart]
    
    def scope(t: Tree, adjustStart: (Int, SourceFile) => Option[Int] = ((i, f) => Some(i)), adjustEnd: (Int, SourceFile) => Option[Int] = ((i, f) => Some(i)) )(body: => Unit): Unit = t match {
      case EmptyTree => 
      case _ =>
        // we only want to adjust the braces if both adjustments were successful. this prevents us from mistakes when there are no braces
        val (start: Int, end: Int) = (adjustStart(t.pos.start, t.pos.source),  adjustEnd(t.pos.end, t.pos.source)) match {
          case (Some(start), Some(end)) => (start, end)
          case _ => (t.pos.start, t.pos.end)
        }
        val newScope = CompositePart(start, end, t.pos.source)
        scopes.top add newScope
        scopes push newScope
        body
        scopes pop
    }
    
    def noChange(offset: Int, file: SourceFile) = offset
    
    def forwardsTo(to: Char)(offset: Int, file: SourceFile): Option[Int] = {
      
      def isWhitespace(c: Char) = c match {
        case '\n' | '\t' | ' ' | '\r' => true
        case _ => false
      }
            
      var i = offset
      // remove the comment
      
      while(isWhitespace(file.content(i))) {
        i += 1
      }
      
      if(file.content(i) == to)
        Some(i + 1) //the end points to the character _after_ the found character
      else
        None
    }
    
    def backwardsTo(to: Char)(offset: Int, file: SourceFile): Option[Int] = {
      
      def isWhitespace(c: Char) = c match {
        case '\n' | '\t' | ' ' | '\r' => true
        case _ => false
      }
      
      if(file.content(offset) == to) 
        return Some(offset)
            
      var i = offset - 1
      // remove the comment
      
      while(isWhitespace(file.content(i))) {
        i -= 1
      }
      
      if(file.content(i) == to)
        Some(i)
      else
        None
    }
        
    val addModifiers = (x: Pair[Long, Position]) => scopes.top add new FlagPart(x._1, x._2)
    
    def visitAll(trees: List[Tree])(separator: Part => Unit ): Unit = trees match {
      case Nil => 
        ()
      case x :: Nil => 
        traverse(x)
      case x :: xs => 
        traverse(x)
        if(scopes.top.trueChildren != Nil)
          separator(scopes.top.trueChildren.last)
        visitAll(xs)(separator)
    }
    
    override def traverse(tree: Tree) = tree match {
      
      case tree if tree.pos == UnknownPosition => 
        println("*woot* a new node!")
        scopes.top add ArtificialTreePart(tree)
        super.traverse(tree)
      
      case tree if !tree.pos.isRange => ()
      
      case t: TypeTree => if(t.original != null) traverse(t.original)
      
      /*case PackageDef(pid, stats) => 
        scope(tree) { scope =>
          super.traverse(tree)
        }*/
      
      case i: Ident =>
        if(i.symbol.hasFlag(Flags.SYNTHETIC)) {
          ()
        } else if (i.symbol.pos == NoPosition)
          scopes.top add new SymTreePart(i) {
            override val end = start + i.name.length
        }
        else
          scopes.top add new SymTreePart(i)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        mods.positions foreach addModifiers
        if (!c.symbol.isAnonymousClass)
          scopes.top add new SymTreePart(c)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) => 
        mods.positions foreach addModifiers
        scopes.top add new SymTreePart(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, typ, rhs) => 
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          mods.positions foreach addModifiers
          scopes.top add new SymTreePart(v)
        }
        super.traverse(tree)

      case select @ Select(qualifier, name)  =>
        traverse(qualifier)
        
        // An ugly hack. when can we actually print the name?
        if (qualifier.isInstanceOf[New]) {
          ()
        } else if(qualifier.isInstanceOf[Super]) {
          scopes.top add new SymTreePart(select) {
            override val end = tree.pos.end
          }
        } else if (qualifier.pos.isRange) {
          scopes.top add new SymTreePart(select) {
            override val start = select.pos.end - select.symbol.nameString.length
            override val end = select.pos.end
          }
        } else {
          scopes.top add new SymTreePart(select)
        }
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        scope(defdef) {
          mods.positions foreach addModifiers
          if(defdef.pos.point >=defdef.pos.start)
            scopes.top add new SymTreePart(defdef)
          
          traverseTrees(mods.annotations)
          traverseTrees(tparams)
          traverseTreess(vparamss)
          traverse(tpt)
          rhs match {
            case rhs: Block => traverse(rhs)
            case _ => scope(rhs, backwardsTo('{'), forwardsTo('}')) {
              traverse(rhs)
            }
          }

        }
        
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

        def withRange(t: Tree) = t.pos.isRange
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        visitAll(classParams) {
          case part: WithRequirement => part.requirePost(", ")
          case _ => throw new Exception("Can't add requirement.")
        }
        
        val(earlyBody, _) = restBody.filter(withRange).partition( (t: Tree) => parents.exists(t.pos precedes _.pos))

        visitAll(earlyBody)(part => ())
        
        visitAll(parents)(part => ())
        
        val trueBody = (restBody -- earlyBody).filter(t => t.pos.isRange || t.pos == UnknownPosition)
        
        if(trueBody.size > 0) {
          scope(tree, ((start, _) => Some((classParams ::: earlyBody ::: (parents filter withRange)).foldLeft(start) ( _ max _.pos.end )))) {
            // fix the start

            visitAll(trueBody) {
              case part: WithRequirement => 
              println("add newline requirement to part "+ part)
              part.requirePost("\n")
              case part => throw new Exception("Can't add requirement to part of type: "+ part.getClass)
            }
          }
        }
        
      case Apply(fun, args) =>
        super.traverse(tree)
        
      case Literal(constant) =>
        scopes.top add new TreePart(tree)
        super.traverse(tree)
        
      case Block(Nil, expr) =>
        super.traverse(tree)
        
      case Block(stats, expr) =>
        if(expr.pos precedes stats.first.pos) {
          traverse(expr)
          super.traverseTrees(stats)
        } else {
          scope (tree, backwardsTo('{'), forwardsTo('}')) {
            super.traverse(tree)
          }
        }
        
      case New(tpt) =>
        scopes.top add new TreePart(tree)
        traverse(tpt)
        
      case s @ Super(qual, mix) =>
        scopes.top add new SymTreePart(s) {
          override val end = tree.pos.end
        }
        super.traverse(tree)
        
      case Match(selector: Tree, cases: List[CaseDef]) =>
        scope(tree) {
          super.traverse(tree)
        }
        
      case _ =>
        println("Not handled: "+ tree.getClass())
        super.traverse(tree)
    }
    
    def visit(tree: Tree) = {
      
      val rootPart = CompositePart(0, tree.pos.source.length, tree.pos.source)
      
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
        
//        println("pairs of children: "+ childrenPairs)
        
        def whitespace(start: Int, end: Int, file: SourceFile) {
          if(start < end) {
            p add WhitespacePart(start, end, file)
          }
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
