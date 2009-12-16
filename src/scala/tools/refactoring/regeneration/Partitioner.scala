package scala.tools.refactoring.regeneration

import scala.tools.refactoring.Tracing

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.{UnknownPosition, InvisiblePosition}

trait Partitioner {
  self: scala.tools.refactoring.Compiler with Tracing with scala.tools.refactoring.LayoutPreferences =>
  import global.{Scope => _, _}
     
  private abstract class Visitor extends Traverser {

    val handle: Contribution
    
    private var scopes = new scala.collection.mutable.Stack[Scope]
      
    private val preRequirements = new ListBuffer[Requisite]
    
    abstract class TreeElement
    case object Mods extends TreeElement
    case object Itself extends TreeElement
    case object Tpt extends TreeElement
    case object Rhs extends TreeElement
    case object ParamList extends TreeElement
    case object ArgsSeparator extends TreeElement
    case object StmtsSeparator extends TreeElement
    case class  ClassParams(ps: List[Tree]) extends TreeElement
    case class  ClassBody(ps: List[Tree]) extends TreeElement
    case object BlockBody extends TreeElement
    
    abstract class Contribution {

      def apply(implicit p: Pair[Tree, TreeElement])
    }
    
    trait FragmentContribution extends Contribution {
      
      private def addFragment(tree: Tree): Fragment = {
        val part = tree match {
          case tree if tree.pos == UnknownPosition => ArtificialTreeFragment(tree)
          case tree: SymTree => SymTreeFragment(tree)
          case _ => TreeFragment(tree)
        }
        addFragment(part)
        part
      }
      
      protected def addFragment(part: Fragment) = scopes.top add part
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        
        case (t: Apply, Itself) => 
          super.apply

        case (i: Ident, Itself) =>
          if(i.symbol.hasFlag(Flags.SYNTHETIC))
            ()
          else if (i.symbol.pos == NoPosition)
            addFragment(new SymTreeFragment(i) {
              override val end = start + i.name.length
            })
          else
            addFragment(i)
          super.apply
            
        case(t @ Select(qualifier, name), Itself) =>
          // An ugly hack. when can we actually print the name?
          if (qualifier.isInstanceOf[New]) {
            ()
          } else if(qualifier.isInstanceOf[Super]) {
            scopes.top add new SymTreeFragment(t) {
              override val end = tree.pos.end
            }
          } else if (qualifier.pos.isRange) {
            scopes.top add new SymTreeFragment(t) {
              override val start = t.pos.end - t.symbol.nameString.length
              override val end = t.pos.end
            }
          } else
            addFragment(t)
          super.apply 
          
         case (s : Super, Itself) =>
           addFragment(new SymTreeFragment(s) {
             override val end = tree.pos.end
           })
          super.apply 
                   
        case (t: Tree, Itself) =>
          addFragment(t) 
          super.apply 
          
        case _ => super.apply 
      }
    }
      
    trait ScopeContribution extends Contribution {
      
      def getIndentation(start: Int, tree: Tree, scope: Scope): Int
      
      private def scope(tree: Tree, indent: Boolean = false, adjustStart: (Int, Seq[Char]) => Option[Int] = noChange, adjustEnd: (Int, Seq[Char]) => Option[Int] = noChange)(body: => Unit) = tree match {
        case tree if tree.pos == UnknownPosition =>
          if(indent) {
            val newScope = new SimpleScope(Some(scopes.top), indentationStep)
            
            scopes.top add newScope
            scopes push newScope
            // klammern zum scope?
            scopes.top.children.head.requireAfter(new Requisite("{", "{\n"))
            body
            scopes.top.children.last.requireBefore(new Requisite("\n", "\n"))
            scopes.top.children.last.requireBefore(new Requisite("}", "}"))
            scopes pop
          } else
            body
            
        case _ =>
          // we only want to adjust the braces if both adjustments were successful. this prevents us from mistakes when there are no braces
          val (start: Int, end: Int) = (adjustStart(tree.pos.start, tree.pos.source.content),  adjustEnd(tree.pos.end, tree.pos.source.content)) match {
            case (Some(start), Some(end)) => (start, end)
            case _ => (tree.pos.start, tree.pos.end)
          }
          
          val newScope = TreeScope(Some(scopes.top), start, end, tree.pos.source, getIndentation(start, tree, scopes.top), tree)
          
          if(preRequirements.size > 0) {
            preRequirements.foreach(newScope requireBefore _)
            preRequirements.clear
          }
          
          scopes.top add newScope
          scopes push newScope
          body
          scopes pop
      }
      
      private def noChange(offset: Int, content: Seq[Char]) = Some(offset) 
      
      import SourceHelper._

      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        
        case (t: DefDef, Rhs) if !t.rhs.isInstanceOf[Block] && t.rhs != EmptyTree =>
          scope(t.rhs, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
            super.apply
          }
          
        case (t @ Block(stats, expr), BlockBody) if !(expr.pos.isRange && (expr.pos precedes stats.head.pos)) =>
          scope (t, indent = true, backwardsSkipLayoutTo('{'), skipLayoutTo('}')) {
            super.apply
          }
          
        case (t: Match, _) =>
          scope(t) {
            super.apply
          }
          
        case (t @ Template(parents, _, body), ClassBody(ts)) if ts.size > 0 =>
          scope(
              t, 
              indent = true, 
              adjustStart = {
                (start, content) =>
                  val abortOn = (ts filter (_.pos.isRange)).map(_.pos.start).foldLeft(content.length)(_ min _)
                  val startFrom = ((body ::: parents filter (_.pos.isRange)) -- ts) .foldLeft(start) ( _ max _.pos.end )
                  forwardsTo('{', abortOn)(startFrom, content)
                }, 
              adjustEnd = noChange) {
            super.apply
          }
          
        case (t @ Apply(fun, args), ParamList) => 
        
          val numArgs = args filter {
            case t: SymTree if t.symbol.hasFlag(Flags.SYNTHETIC) => false
            case t: Tree if t.pos == UnknownPosition => false
            case _ => true
          } size
          
          if(numArgs > 0) {
            val o = backwardsSkipLayoutTo('(')(args.head.pos.start - 1, t.pos.source.content)
            val c = backwardsSkipLayoutTo(')')(t.pos.end, t.pos.source.content) map (1+)/*include the parenthesis*/           
            
            (o, c) match {
              case (openingParenthesis @ Some(_), closingParenthesis @ Some(_)) =>
                scope(
                    t,
                    indent = false,
                    adjustStart = ((_, _) => openingParenthesis),
                    adjustEnd = ((_, _) =>closingParenthesis)
                    ) {
                  super.apply 
                }
              case _ => super.apply
            }
          } else {
            super.apply
          }
          
        case _ => super.apply
      }
    }
      
    type WithModifiers = { def mods: Modifiers; def pos: Position }
    
    trait ModifiersContribution extends FragmentContribution {
      
      def hasModifiers(tree: WithModifiers) = tree.pos match {
        case UnknownPosition => tree.mods.flags != 0 && tree.mods.flags != Flags.PARAM
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
    
    trait RequisitesContribution extends Contribution {
          
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
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        case (t: DefDef, ParamList) =>
          requireAfter("(")
          super.apply
          requireBefore(")")
          
        case (Apply(fun, args), ParamList) if args.size > 0 =>
        
          val needsParenthesis = fun match {
            case Select(qualifier, _) if qualifier.pos == UnknownPosition => false
            case Select(qualifier, _) if !qualifier.pos.isRange => true
            case _ => false
          }
          
          if(needsParenthesis) {
            requireBefore("(")
            super.apply
            requireAfter(")")
          } else
            super.apply
        
        case (_, Tpt) =>
          requireBefore(":", ": ")
          super.apply
          
        case (_, Rhs) =>
          requireAfter("=", " = ")
          super.apply
                    
        case (t: WithModifiers, Mods) =>
          requireAfter(" ", " ")
          super.apply
          
        case (_, ArgsSeparator) =>
          requireAfter(",", ", ")
          super.apply
          
        case (_, StmtsSeparator) =>
          requireAfter("\n", "\n")
          super.apply
          
        case (_, ClassBody(ts)) if ts.size > 0 =>
          super.apply
          requireAfter("\n", "\n")
          
        case(_, Itself) =>
          
          scopes.top.lastChild foreach (x => preRequirements foreach (x.requireBefore))
        
          preRequirements.clear
          super.apply
          
        case _ => 
          super.apply
      }
    }
    
    class BasicContribution extends Contribution {
      
      private def handleList(ts: List[Tree], t: TreeElement): Unit = ts.filter(notEmptyRangeOrUnknown) match {
        case Nil => 
          ()
        case x :: Nil => 
          traverse(x)
        case x :: xs => 
          traverse(x)
          handle(x → t)
          handleList(xs, t)
      }
      
      def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        
        case (t: DefDef, ParamList) =>
          t.vparamss foreach (handleList(_, ArgsSeparator))
          
        case (t: Apply, ParamList) =>
           handleList(t.args, ArgsSeparator)
        
        case (t: Template, ClassParams(ts)) if ts != Nil =>
          handleList(ts, ArgsSeparator)
          
        case (t: Template, ClassBody(ts)) if ts != Nil =>
          handleList(ts, StmtsSeparator)
          
        case (Block(stats, expr), BlockBody) =>
          if(expr.pos.isRange && (expr.pos precedes stats.head.pos))
            handleList(expr :: stats, StmtsSeparator)
          else
            handleList(stats ::: expr :: Nil, StmtsSeparator)
 
        case (t: ValOrDefDef, Tpt) =>
          traverse(t.tpt)
          
        case (t: ValOrDefDef, Rhs) =>
          traverse(t.rhs)  
          
        case (t @ Apply(fun, _), Itself) =>
          traverse(fun)
          handle(t → ParamList)
          
        case (_, Mods) =>
          ()
            
        case x => 
          //println("don't know what to do with "+ x)
      }
    }
    
    def rangeOrUnknown(t: Tree) = t.pos.isRange || t.pos == UnknownPosition
    
    def notEmptyRangeOrUnknown(t: Tree) = t != EmptyTree && rangeOrUnknown(t)  
        
    override def traverse(tree: Tree): Unit = {
    	      
      if(tree.pos != UnknownPosition && !tree.pos.isRange)
        return
      
      tree match {
      
      case t: TypeTree => 
        if(t.original != null)
          traverse(t.original)
        else if(t.pos == UnknownPosition)
          handle(tree, Itself)
        super.traverse(tree)
      
      case i: Ident =>
        handle(tree, Itself)
        super.traverse(tree)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        handle(tree, Mods)
        if (!c.symbol.isAnonymousClass)
          handle(tree, Itself)
        super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) =>
        handle(tree, Mods)
        handle(tree, Itself)
        super.traverse(tree)
        
      case v @ ValDef(mods, name, tpt, rhs) =>
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          handle(tree, Mods)
          handle(tree, Itself)
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
        handle(tree, Itself)
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        
        if((defdef.pos != UnknownPosition && defdef.pos.point >= defdef.pos.start) || defdef.pos == UnknownPosition) {
          handle(tree, Mods)
          
          handle(tree, Itself)
                    
          traverseTrees(mods.annotations)
          traverseTrees(tparams)
          
          if(!vparamss.isEmpty) {
            handle(tree, ParamList)
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
        handle(tree, Itself)
        super.traverse(tree)

      case t @ Template(parents, _, body) =>
        
        val (classParams, restBody) = body.partition {
          case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
          case _ => false
        }
        
        val(earlyBody, _) = restBody.filter(_.pos.isRange).partition( (t: Tree) => parents.exists(t.pos precedes _.pos))
                
        val trueBody = (restBody filterNot (earlyBody contains)).filter(rangeOrUnknown)

        handle(tree → ClassParams(classParams))

        earlyBody foreach traverse
        
        parents foreach traverse
                
        handle(tree → ClassBody(trueBody))

      case Literal(constant) =>
        handle(tree → Itself)
        super.traverse(tree)
        
      case Block(Nil, expr) =>
        super.traverse(tree)
        
      case Block(stats, expr) =>
        handle(tree → BlockBody)

      case New(tpt) =>
        handle(tree → Itself)
        traverse(tpt)
        
      case s @ Super(qual, mix) =>
        handle(tree → Itself)
        super.traverse(tree)
        
      case Match(selector: Tree, cases) =>
        handle(tree → Itself)
        
      case Apply(fun, args) =>
      //scope for ()?
      //  traverse(fun)
        handle(tree → Itself)

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
  
  def essentialFragments(root: Tree, fs: FragmentRepository) = new Visitor {
     val handle = new BasicContribution with RequisitesContribution with ModifiersContribution with ScopeContribution with FragmentContribution {
       def getIndentation(start: Int, tree: Tree, scope: Scope) = SourceHelper.indentationLength(start, tree.pos.source.content) - fs.scopeIndentation(tree).getOrElse(0)
     }
  }.visit(root)
  
  def splitIntoFragments(root: Tree): TreeScope = {
    
    val parts = new Visitor {
      val handle = new BasicContribution with RequisitesContribution with ModifiersContribution with ScopeContribution with FragmentContribution {
        def getIndentation(start: Int, tree: Tree, scope: Scope) = SourceHelper.indentationLength(start, tree.pos.source.content) - scope.indentation
      }
    }.visit(root)
    
    def fillWs(part: Fragment): Fragment = part match {
      case scope: TreeScope => 
      
        val part = scope.copy()
        
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
