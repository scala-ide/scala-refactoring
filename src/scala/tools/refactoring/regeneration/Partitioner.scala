package scala.tools.refactoring.regeneration

import scala.tools.refactoring.Tracing

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.UnknownPosition

trait Partitioner {
  self: scala.tools.refactoring.Compiler with Tracing with scala.tools.refactoring.LayoutPreferences =>
  import global.{Scope => _, _}
  
  private class Visitor(allFragments: Option[FragmentRepository]) extends Traverser {
    
    import SourceHelper._
    
    abstract class TreeElement
    case object Mods extends TreeElement
    case object Tpt extends TreeElement
    case object Rhs extends TreeElement
    case object Vparamss extends TreeElement
    case object ArgsSeparator extends TreeElement
    case object StmtsSeparator extends TreeElement
    
    
    abstract class AstContribution {
      def apply(implicit p: Pair[Tree, TreeElement])
    }
    
    trait ScopeContribution extends AstContribution {
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        case (t: DefDef, Rhs) if !t.rhs.isInstanceOf[Block] =>
          scope(t.rhs, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
            super.apply
          }
        case _ => super.apply
      }
    }    
      
    type WithModifiers = { def mods: Modifiers; def pos: Position }
    
    trait ModifiersContribution extends AstContribution {
      
      def hasModifiers(tree: WithModifiers) = tree.pos match {
        case UnknownPosition => tree.mods.flags != 0
        case _ => tree.mods.positions.size > 0
      }
      
      def modifiers(tree: WithModifiers) = tree.pos match {
        case UnknownPosition=> 
          addFragment(new FlagFragment(tree.mods.flags, UnknownPosition))
        case _ =>
          tree.mods.positions.foreach (x => addFragment(new FlagFragment(x._1, x._2)))
      }
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        case (t: WithModifiers, Mods) =>
          if(hasModifiers(t)) {
            modifiers(t)
            super.apply
          }
        case _ => super.apply
      }
    }
    
    trait RequisitesContribution extends AstContribution {
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        case (t: DefDef, Vparamss) =>
          requireAfter("(")
          super.apply
          requireBefore(")")
          
        case (_, Tpt) =>
          requireBefore(":", ": ")
          super.apply
          
        case (_, Rhs) =>
          requireAfter("=", " = ")
          super.apply
                    
        case (_, Mods) =>
          requireAfter(" ", " ")
          super.apply
          
        case (_, ArgsSeparator) =>
          requireAfter(",", ", ")
          super.apply
          
        case (_, StmtsSeparator) =>
          requireAfter("\n", "\n")
          super.apply
          
        case _ => 
          super.apply
      }
    }
    
    class BasicContribution extends AstContribution {
      def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        
        case (t: DefDef, Vparamss) =>
          t.vparamss foreach (visitAll(_, ArgsSeparator))
 
        case (t: ValOrDefDef, Tpt) =>
          traverse(t.tpt)
          
        case (t: ValOrDefDef, Rhs) =>
          traverse(t.rhs)  
          
        case (_, Mods) =>
          ()
            
        case x => 
          println("don't know what to do with "+ x)
      }
    }
    
    private val handle = new BasicContribution with ScopeContribution with RequisitesContribution with ModifiersContribution
    
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
          val newScope = new SimpleScope(Some(scopes.top), indentationStep)
          
          scopes.top add newScope
          scopes push newScope
          // klammern zum scope?
          scopes.top.children.head.requireAfter(new Requisite("{", "{\n"))
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
              thisIndentation - indentation
            case None => 
              val thisIndentation = SourceHelper.indentationLength(start, tree.pos.source.content)
//              println("part not found, default to 2")
              thisIndentation
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
    
    def visitAll(trees: List[Tree], role: TreeElement): Unit = trees.filter(notEmptyRangeOrUnknown) match {
      case Nil => 
        ()
      case x :: Nil => 
        traverse(x)
      case x :: xs => 
        traverse(x)
        handle(x → role)
        visitAll(xs, role)
    }
    
    override def traverse(tree: Tree): Unit = {
    	
      implicit val currentTree = tree
      
      if(tree.pos != UnknownPosition && !tree.pos.isRange)
        return
      
      tree match {
      
      case t: TypeTree => 
        if(t.original != null)
          traverse(t.original)
        else if(t.pos == UnknownPosition)
          addFragment(t)
      
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
        handle(tree, Mods)
        if (!c.symbol.isAnonymousClass)
          addFragment(c)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) =>
        handle(tree, Mods)
        addFragment(m)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, tpt, rhs) =>
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          handle(tree, Mods)
          addFragment(v)
        }
        traverseTrees(mods.annotations)
        if(tpt.tpe != null && (tpt.pos.isRange || tpt.pos == UnknownPosition) && tpt.tpe != EmptyTree.tpe) {
          handle(tree, Tpt)
        }
        
        if(rhs != EmptyTree) {
          handle(tree, Rhs)
        }

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
        
        if((defdef.pos != UnknownPosition && defdef.pos.point >= defdef.pos.start) || defdef.pos == UnknownPosition) {
          handle(tree, Mods)
          
          addFragment(defdef)
                    
          traverseTrees(mods.annotations)
          traverseTrees(tparams)
          
          if(!vparamss.isEmpty) {
            handle(tree, Vparamss)
          }
          
          if(tpt.pos.isRange || tpt.pos == UnknownPosition) {
            handle(tree, Tpt)  
          }
          
          if(rhs != EmptyTree) {
            handle(tree, Rhs)
          }
 
        } else super.traverse(tree)
        
      case typeDef @ TypeDef(mods: Modifiers, name: Name, tparams, rhs: Tree) =>
        handle(tree, Mods)
        addFragment(typeDef)
        super.traverse(tree)

      case t @ Template(parents, _, body) =>

        def withRange(t: Tree) = t.pos.isRange
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        visitAll(classParams, ArgsSeparator)
        
        val(earlyBody, _) = restBody.filter(withRange).partition( (t: Tree) => parents.exists(t.pos precedes _.pos))

        earlyBody foreach traverse
        
        parents foreach traverse
        
        val trueBody = (restBody filterNot (earlyBody contains)).filter(rangeOrUnknown)
        
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
            visitAll(trueBody, StmtsSeparator)
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
                
        if(expr.pos.isRange && (expr.pos precedes stats.head.pos)) {
          visitAll(expr :: stats, StmtsSeparator)
        } else {
          scope (tree, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
            visitAll(stats ::: expr :: Nil, StmtsSeparator)
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
        
      case Match(selector: Tree, cases) =>
        scope(tree) {
          super.traverse(tree)
        }
        
      case Apply(fun, args) =>
      //scope for ()?
        traverse(fun)
        if(args.size > 1) {
          requireAfter("(")
          visitAll(args, ArgsSeparator)
          requireAfter(")")
        } else if(args.size > 0) {
          traverse(args.head)
        }
        
      case _ =>
        super.traverse(tree)
    }}
    
    def visit(tree: Tree) = {
      
      val rootFragment = TreeScope(None, 0, tree.pos.source.length, tree.pos.source, 0, tree)

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
