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
        
    private var scopes = new scala.collection.mutable.Stack[CompositePart]
    
    private val preRequirements = new ListBuffer[Required]
    
    def requirePre(check: String): Unit = requirePre(check, check)
    def requirePre(check: String, write: String) = preRequirements += Required(check, write)
    
    def requirePost(check: String): Unit = requirePost(check, check)
    def requirePost(check: String, write: String) = {
      if(preRequirements.size > 0)
        requirePre(check, write)
      else
        scopes.top.trueChildren.last.requirePost(new Required(check, write))
    }
    
    def requirePostOrPre(r: String) = {
      if(scopes.top.trueChildren == Nil)
        requirePre(r)
      else
        requirePost(r, r)
    }
    
    def scope(t: Tree, adjustStart: (Int, SourceFile) => Option[Int] = ((i, f) => Some(i)), adjustEnd: (Int, SourceFile) => Option[Int] = ((i, f) => Some(i)) )(body: => Unit) = t match {
      case EmptyTree => ()
      case tree if tree.pos == UnknownPosition => 
        body
      case tree if !tree.pos.isRange =>
        ()
      case _ =>
        // we only want to adjust the braces if both adjustments were successful. this prevents us from mistakes when there are no braces
        val (start: Int, end: Int) = (adjustStart(t.pos.start, t.pos.source),  adjustEnd(t.pos.end, t.pos.source)) match {
          case (Some(start), Some(end)) => (start, end)
          case _ => (t.pos.start, t.pos.end)
        }
        
        val newScope = CompositePart(start, end, t.pos.source, t.getClass.getSimpleName)
        
        if(preRequirements.size > 0) {
          preRequirements.foreach(newScope requirePre _)
          preRequirements.clear
        }
        
        scopes.top add newScope
        scopes push newScope
        body
        scopes pop
    }
    
    def noChange(offset: Int, file: SourceFile) = Some(offset)
    
    def forwardsTo(to: Char, max: Int)(offset: Int, file: SourceFile): Option[Int] = {
      var i = offset
      
      while(file.content(i) != to && i < max) {
        i += 1
      }
      
      if(file.content(i) == to)
        Some(i)
      else
        None
    }
    
    def skipWhitespaceTo(to: Char)(offset: Int, file: SourceFile): Option[Int] = {
      
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
    
    def backwardsSkipWhitespaceTo(to: Char)(offset: Int, file: SourceFile): Option[Int] = {
      
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
        
    def addModifiers(x: Pair[Long, Position]) = scopes.top add new FlagPart(x._1, x._2)
    def addPart(tree: Tree): Part = {
      val part = tree match {
        case tree if tree.pos == UnknownPosition => ArtificialTreePart(tree)
        case tree: SymTree => SymTreePart(tree)
        case _ => TreePart(tree)
      }
      addPart(part)
      part
    }
    
    def addPart(part: Part) = {
      scopes.top add part
      preRequirements.foreach(part requirePre _)
      preRequirements.clear
    }
    
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
    
    override def traverse(tree: Tree): Unit = {
      
      if(tree.pos != UnknownPosition && !tree.pos.isRange)
        return
      
      tree match {
      
      case t: TypeTree => 
        if(t.original != null) 
          traverse(t.original)
        else if(t.pos == UnknownPosition)
          addPart(t)
      
      /*case PackageDef(pid, stats) => 
        scope(tree) {
          super.traverse(tree)
        }*/
      
      case i: Ident =>
        if(i.symbol.hasFlag(Flags.SYNTHETIC))
          ()
        else if (i.symbol.pos == NoPosition)
          addPart(new SymTreePart(i) {
            override val end = start + i.name.length
          })
        else
          addPart(i)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        mods.positions foreach addModifiers
        if (!c.symbol.isAnonymousClass)
          addPart(c)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) => 
        mods.positions foreach addModifiers
        addPart(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, tpt, rhs) => 
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          mods.positions foreach addModifiers
          addPart(v)
        }
        traverseTrees(mods.annotations)
        if(tpt.pos.isRange || tpt.pos == UnknownPosition) {
          requirePre(":", ": ")
          traverse(tpt)
        }
        traverse(rhs)

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
        } else
          addPart(select)
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        scope(defdef) {
          mods.positions foreach addModifiers
          if((defdef.pos != UnknownPosition && defdef.pos.point >= defdef.pos.start) || defdef.pos == UnknownPosition) {
            requirePostOrPre(" ")
            addPart(defdef)
                      
            traverseTrees(mods.annotations)
            traverseTrees(tparams)
            
            if(!vparamss.isEmpty) {
              requirePost("(")
	            traverseTreess(vparamss)
              requirePre(")")
            }
            
            if(tpt.pos.isRange || tpt.pos == UnknownPosition) {
              requirePre(":", ": ")
              traverse(tpt)              
            }
            
            rhs match {
              case EmptyTree =>
              case rhs: Block =>
                requirePost("=", " = ")
                traverse(rhs) //Block creates its own Scope
              case _ =>
                requirePost("=", " = ")
                scope(rhs, backwardsSkipWhitespaceTo('{'), skipWhitespaceTo('}')) {
                traverse(rhs)
              }
            }
          } else
            super.traverse(tree)
        }
        
      case typeDef @ TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =>
          mods.positions foreach addModifiers
          addPart(typeDef)
          super.traverse(tree)

      case t @ Template(parents, _, body) =>

        def withRange(t: Tree) = t.pos.isRange
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        visitAll(classParams) {
          case part: WithRequirement => part.requirePost(Required(",", ", "))
          case _ => throw new Exception("Can't add requirement.")
        }
        
        val(earlyBody, _) = restBody.filter(withRange).partition( (t: Tree) => parents.exists(t.pos precedes _.pos))

        visitAll(earlyBody)(part => ())
        
        visitAll(parents)(part => ())
        
        val trueBody = (restBody -- earlyBody).filter(t => t.pos.isRange || t.pos == UnknownPosition)
        
        if(trueBody.size > 0) {
          scope(tree, {
            (start, file) =>
              val abortOn = (trueBody filter withRange) map (_.pos.start) reduceLeft (_ min _)
              val startFrom = (classParams ::: earlyBody ::: (parents filter withRange)).foldLeft(start) ( _ max _.pos.end )
              forwardsTo('{', abortOn)(startFrom, file)
            }) {
            
            // fix the start

            visitAll(trueBody) {
              case part: WithRequirement => part.requirePost(new Required("\n"))
              case part => throw new Exception("Can't add requirement to part of type: "+ part.getClass)
            }
          }
        }
        
      case Literal(constant) =>
        addPart(tree)
        super.traverse(tree)
        
      case Block(Nil, expr) =>
        super.traverse(tree)
        
      case Block(stats, expr) =>
        if(expr.pos.isRange && (expr.pos precedes stats.first.pos)) {
          traverse(expr)
          super.traverseTrees(stats)
        } else {
          scope (tree, backwardsSkipWhitespaceTo('{'), skipWhitespaceTo('}')) {
            super.traverse(tree)
          }
        }
        
      case New(tpt) =>
        addPart(tree)
        traverse(tpt)
        
      case s @ Super(qual, mix) =>
        addPart(new SymTreePart(s) {
          override val end = tree.pos.end
        })
        super.traverse(tree)
        
      case Match(selector: Tree, cases: List[CaseDef]) =>
        scope(tree) {
          super.traverse(tree)
        }
        
      case _ =>
        println("Not handled: "+ tree.getClass())
        super.traverse(tree)
    }}
    
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
