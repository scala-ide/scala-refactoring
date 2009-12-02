package scala.tools.refactoring.regeneration

import scala.tools.refactoring.Tracing

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.UnknownPosition

trait Partitioner {
  self: scala.tools.refactoring.Compiler with Tracing =>
  import global.{Scope => _, _}
  
  private class Visitor(allFragments: Option[FragmentRepository]) extends Traverser {
    
    import SourceHelper._
    
    private var scopes = new scala.collection.mutable.Stack[Scope]
    
    private val preRequirements = new ListBuffer[Requisite]
    
    def requireBefore(check: String): Unit = requireBefore(check, check)
    def requireBefore(check: String, write: String) = preRequirements += Requisite(check, write)
    
    def requireAfter(check: String): Unit = requireAfter(check, check)
    def requireAfter(check: String, write: String) = {
      if(preRequirements.size > 0)
        requireBefore(check, write)
      else
        scopes.top.lastChild match {
        case Some(part) => part.requireAfter(new Requisite(check, write))
        case None => //throw new Exception("No place to attach requisite"+ check +"!") //??
      }
    }
    
    def requireAfterOrBefore(r: String) = scopes.top.lastChild match {
      case Some(_) => requireAfter(r, r)
      case _ => requireBefore(r)
    }

    def scope(tree: Tree, indent: Boolean = false, adjustStart: (Int, Seq[Char]) => Option[Int] = noChange, adjustEnd: (Int, Seq[Char]) => Option[Int] = noChange)(body: => Unit) = tree match {
      case EmptyTree => ()
      case tree if tree.pos == UnknownPosition =>
        if(indent) {
          val newScope = new SimpleScope(Some(scopes.top), 2)
          
          scopes.top add newScope
          scopes push newScope
          requireBefore("{", "{\n")
          body
          requireAfter("}", "\n}")
          scopes pop
        } else
          body
        
      case tree if !tree.pos.isRange =>
        ()
      case _ =>
        // we only want to adjust the braces if both adjustments were successful. this prevents us from mistakes when there are no braces
        val (start: Int, end: Int) = (adjustStart(tree.pos.start, tree.pos.source.content),  adjustEnd(tree.pos.end, tree.pos.source.content)) match {
          case (Some(start), Some(end)) => (start, end)
          case _ => (tree.pos.start, tree.pos.end)
        }
        
        val i = allFragments match {
          
          case Some(allFragments) => allFragments.scopeIndentation(tree) match {
        
            case Some(indentation) => 
              val thisIndentation = SourceHelper.indentationLength(start, tree.pos.source.content)
//              println("!!! tree has an indentation of: "+ (thisIndentation - indentation))
              allFragments.scopeIndentation(tree)
              thisIndentation - indentation
            case None => 
//              println("part not found, default to 2")
              2
          }
          case None => 
//            println("top indentation is: "+ scopes.top.indentation) 
//            println("my indentation is: "+ SourceHelper.indentationLength(start, tree.pos.source.content))
//            println("found nothing in the partsholder for tree"+ tree.pos +", so take "+ (SourceHelper.indentationLength(start, tree.pos.source.content) - scopes.top.indentation))
            SourceHelper.indentationLength(start, tree.pos.source.content) - scopes.top.indentation
        }
        
        val newScope = TreeScope(Some(scopes.top), start, end, tree.pos.source, i, tree)
        
        if(preRequirements.size > 0) {
          preRequirements.foreach(newScope requireBefore _)
          preRequirements.clear
        }
        
        scopes.top add newScope
        scopes push newScope
        body
        scopes pop
    }
    
    def noChange(offset: Int, content: Seq[Char]) = Some(offset) 
        
    def modifiers(tree: { def mods: Modifiers; def pos: Position }) = tree.pos match {
      case UnknownPosition => addFragment(new FlagFragment(tree.mods.flags, UnknownPosition))
      case _ => tree.mods.positions.foreach (x => addFragment(new FlagFragment(x._1, x._2)))
    }
        
    def addFragment(tree: Tree): Fragment = {
      val part = tree match {
        case tree if tree.pos == UnknownPosition => ArtificialTreeFragment(tree)
        case tree: SymTree => SymTreeFragment(tree)
        case _ => TreeFragment(tree)
      }
      addFragment(part)
      part
    }
    
    def addFragment(part: Fragment) = {
      scopes.top add part
      preRequirements.foreach(part requireBefore _)
      preRequirements.clear
    }
        
    def rangeOrUnknown(t: Tree) = t.pos.isRange || t.pos == UnknownPosition
    def notEmptyRangeOrUnknown(t: Tree) = t != EmptyTree && rangeOrUnknown(t)  
    
    def visitAll(trees: List[Tree])(separator: Fragment => Unit ): Unit = trees.filter(notEmptyRangeOrUnknown) match {
      case Nil => 
        ()
      case x :: Nil => 
        traverse(x)
      case x :: xs => 
        traverse(x)
        scopes.top.lastChild match {
          case Some(part) => separator(part)
          case _ => ()
        }
          
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
          addFragment(t)
      
      /*case PackageDef(pid, stats) => 
        scope(tree) {
          super.traverse(tree)
        }*/
      
      case i: Ident =>
        if(i.symbol.hasFlag(Flags.SYNTHETIC))
          ()
        else if (i.symbol.pos == NoPosition)
          addFragment(new SymTreeFragment(i) {
            override val end = start + i.name.length
          })
        else
          addFragment(i)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        modifiers(c)
        if (!c.symbol.isAnonymousClass)
          addFragment(c)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) => 
        modifiers(m)
        addFragment(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, tpt, rhs) => 
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          modifiers(v)
          addFragment(v)
        }
        traverseTrees(mods.annotations)
        if(tpt.pos.isRange || tpt.pos == UnknownPosition) {
          requireBefore(":", ": ")
          traverse(tpt)
        }
        traverse(rhs)

      case select @ Select(qualifier, name)  =>
        traverse(qualifier)
        
        // An ugly hack. when can we actually print the name?
        if (qualifier.isInstanceOf[New]) {
          ()
        } else if(qualifier.isInstanceOf[Super]) {
          scopes.top add new SymTreeFragment(select) {
            override val end = tree.pos.end
          }
        } else if (qualifier.pos.isRange) {
          scopes.top add new SymTreeFragment(select) {
            override val start = select.pos.end - select.symbol.nameString.length
            override val end = select.pos.end
          }
        } else
          addFragment(select)
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        modifiers(defdef)
        if((defdef.pos != UnknownPosition && defdef.pos.point >= defdef.pos.start) || defdef.pos == UnknownPosition) {
          requireAfterOrBefore(" ")
          addFragment(defdef)
                    
          traverseTrees(mods.annotations)
          traverseTrees(tparams)
          
          if(!vparamss.isEmpty) {
            requireAfter("(")
            vparamss foreach (visitAll(_)(_.requireAfter(Requisite(",", ", "))))
            requireBefore(")")
          }
          
          if(tpt.pos.isRange || tpt.pos == UnknownPosition) {
            requireBefore(":", ": ")
            traverse(tpt)              
          }
          
          rhs match {
            case EmptyTree =>
            case rhs: Block =>
              requireAfter("=", " = ")
              traverse(rhs) //Block creates its own Scope
            case _ =>
              requireAfter("=", " = ")
              scope(rhs, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
                traverse(rhs)
              }
          }
        } else super.traverse(tree)
        
      case typeDef @ TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =>
        modifiers(typeDef)
        addFragment(typeDef)
        super.traverse(tree)

      case t @ Template(parents, _, body) =>

        def withRange(t: Tree) = t.pos.isRange
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        visitAll(classParams) {
          case part: WithRequisite => part.requireAfter(Requisite(",", ", "))
          case _ => throw new Exception("Can't add requirement.")
        }
        
        val(earlyBody, _) = restBody.filter(withRange).partition( (t: Tree) => parents.exists(t.pos precedes _.pos))

        visitAll(earlyBody)(part => ())
        
        visitAll(parents)(part => ())
        
        val trueBody = (restBody -- earlyBody).filter(rangeOrUnknown)
        
        if(trueBody.size > 0) {
          scope(
              tree, 
              indent = true, 
              adjustStart = {
                (start, content) =>
                  val abortOn = (trueBody filter withRange).map(_.pos.start).foldLeft(content.length)(_ min _)
                  val startFrom = (classParams ::: earlyBody ::: (parents filter withRange)).foldLeft(start) ( _ max _.pos.end )
                  forwardsTo('{', abortOn)(startFrom, content)
                }, 
            adjustEnd = noChange) {
            visitAll(trueBody)(_.requireAfter(new Requisite("\n")))
            requireAfter("\n")
          }
        } else {
          // there might be an empty body that needs a scope :-(
        }
        
      case Literal(constant) =>
        addFragment(tree)
        super.traverse(tree)
        
      case Block(Nil, expr) =>
        super.traverse(tree)
        
      case Block(stats, expr) =>
        
        val newline: Fragment => Unit = _.requireAfter(new Requisite("\n"))
        
        if(expr.pos.isRange && (expr.pos precedes stats.first.pos)) {
          traverse(expr)
          visitAll(stats)(newline)
        } else {
          scope (tree, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
            visitAll(stats ::: expr :: Nil)(newline)
          }
        }
        
      case New(tpt) =>
        addFragment(tree)
        traverse(tpt)
        
      case s @ Super(qual, mix) =>
        addFragment(new SymTreeFragment(s) {
          override val end = tree.pos.end
        })
        super.traverse(tree)
        
      case Match(selector: Tree, cases: List[CaseDef]) =>
        scope(tree) {
          super.traverse(tree)
        }
        
      case _ =>
        super.traverse(tree)
    }}
    
    def visit(tree: Tree) = {
      
      val rootFragment = TreeScope(None, 0, tree.pos.source.length, tree.pos.source, /*SourceHelper.indentationLength(tree)*/0, tree)

      scopes push rootFragment
      
      traverse(tree)

      rootFragment
    }
  }
  
  def essentialFragments(root: Tree, allFragments: FragmentRepository) = new Visitor(Some(allFragments)).visit(root)
  
  def splitIntoFragments(root: Tree): TreeScope = {
    
    val parts = new Visitor(None).visit(root)
    
    def fillWs(part: Fragment): Fragment = part match {
      case scope: TreeScope => 
      
        val part = TreeScope(scope.parent, scope.start, scope.end, scope.file, scope.relativeIndentation, scope.tree)
        
        def layout(start: Int, end: Int, file: SourceFile) {
          if(start < end) {
            part add LayoutFragment(start, end, file)
          }
        }

        (scope.children zip scope.children.tail) foreach {
          case (left: TreeScope#BeginOfScope, right: OriginalSourceFragment) => ()
            layout(left.end, right.start, left.file)
          case (left: OriginalSourceFragment, right: OriginalSourceFragment) =>
            part add (fillWs(left))
            layout(left.end, right.start, left.file)
        }
        part
      case _ => part
    }
    
    fillWs(parts).asInstanceOf[TreeScope]
  }
}
