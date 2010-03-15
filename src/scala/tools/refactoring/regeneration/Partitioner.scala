package scala.tools.refactoring.regeneration

import scala.tools.refactoring.common._

import scala.tools.nsc.util._
import scala.tools.nsc.ast._
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.{Flags, Names, Symbols}
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.interactive.Global

trait PartitionerContributions {
  
  val global: Global
  import global._
  
  object TreeRoles {
    sealed abstract class TreeElement
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
    case object Cond extends TreeElement
    case object Then extends TreeElement
    case object Else extends TreeElement
    case object Name extends TreeElement      
  }
  
  import TreeRoles._
  
  abstract class Contribution {
    def apply(implicit p: Pair[Tree, TreeElement])
  }
}

trait Partitioner extends PartitionerContributions {
  self: Tracing with LayoutPreferences with Fragments with FragmentRepository with SourceHelper =>
  val global: Global
  import global.{Scope => _, _}
     
  private abstract class Visitor extends Traverser {

    import TreeRoles._
  
    val handle: Contribution
    
    private var rootScope: TreeScope = _
    
    private var currentScope: Scope = _ 
      
    private val preRequirements = new ListBuffer[Requisite]

    trait FragmentContribution extends Contribution {
      
      private def addFragment(tree: Tree): Fragment = {
        val part = tree match {
          case tree if tree.pos == NoPosition => ArtificialTreeFragment(tree)
          case tree: SymTree => SymTreeFragment(tree)
          case tree: New => new TreeFragment(tree) {
            override lazy val start = tree.pos.start min tree.pos.point
            override lazy val end = start + "new".length
          }
          case _ => TreeFragment(tree)
        }
        addFragment(part)
        part
      }
      
      protected def addFragment(part: Fragment) = currentScope add part
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        
        case (t: Apply, Itself) => 
          super.apply

        case (i: Ident, Itself) =>
          if(i.symbol.hasFlag(Flags.SYNTHETIC))
            ()
          else if (i.pos.isRange && i.pos.start == i.pos.end)
            ()
          else if (i.symbol.pos == NoPosition)
            addFragment(new SymTreeFragment(i) {
              override val end = start + i.name.length
            })
          else
            addFragment(i)
          super.apply
          
        case(Select(qualifier: New, _), Itself) =>
          super.apply 
          
        case(Select(_, name), Itself) if name.toString == "apply" =>
          super.apply 
            
        case(t @ Select(qualifier, name), Itself) =>
         
          if (qualifier.pos.isRange && qualifier.pos.start > t.pos.start) /* e.g. !true */ {
            currentScope add new SymTreeFragment(t) {
              override val start = t.pos.start
              override val end = qualifier.pos.start
            }
          } else if (qualifier.pos.isRange) {
            currentScope add new SymTreeFragment(t) {
              override val start = t.pos.point.max(qualifier.pos.end + 1)
              override val end = t.pos.end
            }
          } else {
            addFragment(t)
          }
          super.apply 
          
         case (s: Super, Itself) =>
           addFragment(new SymTreeFragment(s) {
             override val end = tree.pos.end
           })
          super.apply 
          
        case (t: ImplDef, Itself) =>
          super.apply
          
        case (t: ImplDef, Name) =>
          addFragment(t)
          super.apply
          
        case (t: Bind, Itself) =>
          addFragment(t)
          super.apply
          
        case (t @ Import(expr, selectors), Itself) =>
          super.apply
          
          if(selectors.size > 1) {
            currentScope.lastChild map (_.requireAfter(new Requisite("{")))
          }
          
          addFragment(ImportSelectorsFragment(selectors, t.pos.source))
          
          if(selectors.size > 1) {
            currentScope.lastChild map (_.requireAfter(new Requisite("}")))
          }
          
        case (t: Match, Itself) =>
          super.apply
          
        case (t: Tree, Itself) =>
          addFragment(t)
          super.apply
          
        case _ => super.apply 
      }
    }
      
    trait ScopeContribution extends Contribution {
      
      private def enterScope(s: Scope)(body: => Unit) = {
        val oldScope = currentScope
        oldScope add s
        currentScope = s
        body
        currentScope = oldScope
      }
      
      def getIndentation(start: Int, tree: Tree, scope: Scope): Int
      
      private def scope(tree: Tree, indent: Boolean = false, adjustStart: (Int, SourceFile) => Option[Int] = noChange, adjustEnd: (Int, SourceFile) => Option[Int] = noChange)(body: => Unit) = tree match {
        case tree if tree.pos == NoPosition =>
          if(indent) {
            enterScope(new SimpleScope(Some(currentScope), indentationStep)) {
              currentScope.children.head.requireAfter(new Requisite("{", "{\n"))
              body
              currentScope.children.last.requireBefore(new Requisite("\n", "\n"))
              currentScope.children.last.requireBefore(new Requisite("}", "}"))
            }
          } else {
            body
          }
            
        case _ =>
          // we only want to adjust the braces if both adjustments were successful. this prevents us from mistakes when there are no braces
          val (start: Int, end: Int) = (adjustStart(tree.pos.start, tree.pos.source),  adjustEnd(tree.pos.end, tree.pos.source)) match {
            case (Some(start), Some(end)) => (start, end)
            case _ => (tree.pos.start, tree.pos.end)
          }
          
          val indentation = getIndentation(start, tree, currentScope)
                    
          val newScope = TreeScope(Some(currentScope), start, if(end + 1 == tree.pos.source.content.length) end + 1 else end, tree.pos.source, indentation, tree)
          
          if(preRequirements.size > 0) {
            preRequirements.foreach(newScope requireBefore _)
            preRequirements.clear
          }
          
          if(currentScope == newScope) {
            body
          } else {
            enterScope(newScope)(body)
          }
      }
      
      private def noChange(offset: Int, content: SourceFile) = Some(offset) 
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
      
        case (t: ImplDef, Itself) =>
          scope(
              t, 
              indent = false, 
              (_, _) => {
                val minimum = (t.pos.start /: t.mods.positions.map(_._2.start)) (_ min _)
                Some(minimum)
              },
              noChange) {
            super.apply
          }
        
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

        case (t: If, Cond) =>
          scope(
              t.cond, 
              indent = false,
              (s, c) => backwardsSkipLayoutTo('(')(s, c),
              (s, c) => skipLayoutTo(')')(s, c)) {
            super.apply
          }
                    
        case (t: If, Then) if indentationLength(t.thenp) > indentationLength(t) =>
          scope(
              t.thenp, 
              indent = true,
              (s, c) => backwardsSkipLayoutTo('{')(s, c) orElse backwardsSkipLayoutTo('\n')(s, c),
              (s, c) => skipLayoutTo('}')(s, c) orElse (skipLayoutTo('\n')(s, c) map (_ - 1))) {
            super.apply
          }  
          
        case (t: If, Else) if indentationLength(t.elsep) > indentationLength(t) =>
          scope(
              t.elsep,
              indent = true,
              (s, c) => backwardsSkipLayoutTo('{')(s, c) orElse backwardsSkipLayoutTo('\n')(s, c),
              (s, c) => skipLayoutTo('}')(s, c) orElse (skipLayoutTo('\n')(s, c) map (_ - 1))) {
            super.apply
          }
          
        case (t @ Template(parents, _, body), ClassBody(ts)) if !ts.isEmpty =>
          scope(
              t, 
              indent = true, 
              adjustStart = {
                (start, content) =>
                  val abortOn =  (content.length /: (ts filter (_.pos.isRange)).map(_.pos.start)) (_ min _)
                  val startFrom = (start /: ((body ::: parents filter (_.pos.isRange)) filterNot (ts contains))) (_ max _.pos.end)
                  forwardsTo('{', abortOn)(startFrom, content)
                }, 
              adjustEnd = noChange) {
            super.apply
          }
          
        case (t @ Apply(fun, args), ParamList) => 
                  
          val actualArguments = args filter {
            case arg: SymTree if arg.symbol.hasFlag(Flags.SYNTHETIC) => false
            case arg => arg.pos.isRange || arg.pos == NoPosition
          }
        
          val numArgs = actualArguments filter (_.pos.isRange) size
          
          if(numArgs == 0 && ! actualArguments.isEmpty) {
            enterScope(new SimpleScope(Some(currentScope), 0))(super.apply)
          } else if(numArgs > 0) {
            val o = backwardsSkipLayoutTo('(')(actualArguments.head.pos.start - 1, t.pos.source)
            val c = backwardsSkipLayoutTo(')')(t.pos.end, t.pos.source) map (1+)/*include the parenthesis*/
            
            (o, c) match {
              case (openingParenthesis @ Some(_), closingParenthesis @ Some(_)) =>
                scope(
                    t,
                    indent = false,
                    adjustStart = (_, _) => openingParenthesis,
                    adjustEnd = (_, _) =>closingParenthesis
                    ) {
                  super.apply 
                }
              case _ => 
                super.apply
            }
          } else {
            super.apply
          }
          
        case _ => super.apply
      }
    }
    
    trait ModifiersContribution extends FragmentContribution {
            
      def hasModifiers(tree: MemberDef) = tree.pos match {
        case NoPosition => tree.mods.flags != 0 && tree.mods.flags != Flags.PARAM
        case _ => tree.mods.positions.size > 0
      }
      
      def modifiers(tree: MemberDef) = tree.pos match {
        case NoPosition=> 
          addFragment(new Fragment with Flag {
            val flag = tree.mods.flags
          })
        case _ => tree.mods.positions.foreach {
          case (flag, pos) if rootScope.start <= pos.start && pos.end <= rootScope.end =>
            addFragment(new FlagFragment(flag, pos))
          case _ => ()
        }
      }
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
        case (t: MemberDef, Mods) =>
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
          currentScope.lastChild match {
            case Some(part) => part.requireAfter(new Requisite(check, write))
            case None => //throw new Exception("No place to attach requisite «"+ check +"»!")
          }
      }
      
      def requireAfterOrBefore(r: String) = currentScope.lastChild match {
        case Some(_) => requireAfter(r, r)
        case _ => requireBefore(r)
      }
      
      abstract override def apply(implicit p: Pair[Tree, TreeElement]) = p match {
      
        case (t: DefDef, ParamList) =>
          requireBefore("(")
          super.apply
          requireAfter(")")
          
        case (Apply(fun, args), ParamList) if args.size > 0 =>
        
          val needsParenthesis = fun match {
            case TypeApply(Select(Select(qualifier, name), _), _) if name.toString matches """Tuple\d+""" => true
            case Select(qualifier, _) if qualifier.pos == NoPosition => false
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
                  
        case (t: DefDef, Rhs) =>
          requireAfter("=", " = ")
          super.apply 
          
        case (_, Rhs) =>
          requireAfter("=", " = ")
          super.apply
                    
        case (t: MemberDef, Mods) =>
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
          
        case (t: If, Cond) =>
          requireAfter("(")
          super.apply
          requireAfter(")")
          
        case(_, Itself) =>
          
          currentScope.lastChild foreach (x => preRequirements foreach (x.requireBefore))
        
          preRequirements.clear
          super.apply
          
        case _ => 
          super.apply
      }
    }
    
    abstract class BasicContribution extends Contribution {
      
      val global: Global
      
      private def handleMultipleAssignments(ts: List[Tree]): List[Tree] = ts match {
        case (x @ ValDef(_, _, _, Match(Typed(expr, _), _))) :: xs if x.mods.hasFlag(Flags.LOCAL | Flags.PRIVATE | Flags.SYNTHETIC) =>
          
          val numberOfAssignments = expr.tpe.typeArgs.size
          
          val (assignments, rest) = xs.splitAt(numberOfAssignments)
                  
          /* Values have a "val" modifier which we need to remove, except on the first one,
           * otherwise we get something like val (a, val b) = ...
           */
          def removeAllMods(ts: List[Tree]) = ts.head :: ts.tail.map {
            case v: ValDef => v.copy(mods = NoMods) copyAttrs (v)
          }
          
          removeAllMods(assignments) ::: expr :: handleMultipleAssignments(rest)
        case ts => ts
      }
      
      private def handleList(ts: List[Tree], t: TreeElement): Unit = handleMultipleAssignments(ts.filter(notEmptyRangeOrUnknown)) match {
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
          
        case (t: TypeApply, ParamList) =>
          handleList(t.args, ArgsSeparator)
          
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
          
        case (t: If, Cond) =>
          traverse(t.cond)
          
        case (t: If, Then) =>
          traverse(t.thenp)
          
        case (t: If, Else) =>
          traverse(t.elsep)
          
        case (t: Import, Itself) =>
          traverse(t.expr)
          
        case (t: ModuleDef, Itself) =>
          handle(t → Mods)
          handle(t → Name)
          traverseTrees(t.mods.annotations)
          traverse(t.impl)  
          
        case (t: ClassDef, Itself) =>
          handle(t → Mods)
          handle(t → Name)
          traverseTrees(t.mods.annotations)
          traverseTrees(t.tparams)
          traverse(t.impl)
          
        case (t: Match, Itself) =>
          traverse(t.selector)
          traverseTrees(t.cases)
            
        case x => 
          //println("don't know what to do with "+ x)
      }
    }
    
    def rangeOrNoPos(t: Tree) = t.pos.isRange || t.pos == NoPosition
    
    def notEmptyRangeOrUnknown(t: Tree) = t != EmptyTree && rangeOrNoPos(t)  
        
    final override def traverse(tree: Tree): Unit = tree match {
        
      case t if !rangeOrNoPos(t) => 
        return // tree is a compiler generated tree, we don't have to handle them
      
      case t: TypeTree => 
        if(t.original != null)
          traverse(t.original)
        else if(t.pos == NoPosition && t.tpe != null)
          handle(tree, Itself)
        super.traverse(tree)
      
      case i: Ident =>
        handle(tree, Itself)
        super.traverse(tree)
        
      case c @ ClassDef(mods, name, tparams, impl) =>
        if (!c.symbol.isAnonymousClass)
          handle(tree, Itself)
        else
          super.traverse(tree)
        
      case m @ ModuleDef(mods, name, impl) =>
        handle(tree, Itself)
        
      case v @ ValDef(mods, name, tpt, rhs) =>
        if(!v.symbol.hasFlag(Flags.SYNTHETIC)) {
          handle(tree, Mods)
          handle(tree, Itself)
        }
        traverseTrees(mods.annotations)
        // XXX simplify these two checks
        if(tpt.tpe != null && (tpt.pos.isRange || tpt.pos == NoPosition) && tpt.tpe != EmptyTree.tpe) {
          handle(tree, Tpt)
        }
        
        if(rhs != EmptyTree && (rhs.pos.isRange || rhs.pos == NoPosition)) {
          handle(tree, Rhs)
        }

      case select @ Select(qualifier, name) =>
        if(qualifier.pos.isRange && select.pos.isRange && qualifier.pos.start > select.pos.start) /*e.g. !true */ {
          handle(tree, Itself)
          traverse(qualifier)
        } else {
          traverse(qualifier)
          handle(tree, Itself) 
        }
        
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        
        if((defdef.pos != NoPosition && defdef.pos.point >= defdef.pos.start) || defdef.pos == NoPosition) {
          handle(tree, Mods)
          
          handle(tree, Itself)
                    
          traverseTrees(mods.annotations)
          traverseTrees(tparams)
          
          if(!vparamss.isEmpty) {
            handle(tree, ParamList)
          }
          
          if(tpt.pos.isRange || tpt.pos == NoPosition) {
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
                
        val trueBody = (restBody filterNot (earlyBody contains)).filter(rangeOrNoPos).filter(_ != EmptyTree)

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
        
      case Match(selector, cases) =>
        handle(tree → Itself)
        
      case Import(expr, selectors) =>
        handle(tree → Itself)
        
      case Apply(fun, args) =>
        handle(tree → Itself)
        
      case t @ If(cond, thenp, elsep) =>
        handle(tree → Cond)
        handle(tree → Then)
        if(t.elsep.pos.isRange)
          handle(tree → Else)
          
      case CaseDef(pat, guard, body) =>
        traverse(pat)
        if(guard != EmptyTree)
          traverse(guard)
        traverse(body)
        
      case t @ Bind(name, body) =>
        handle(t → Itself)
        traverse(body)
        
      case t @ Typed(expr, tpt) =>
        traverse(expr)
        handle(t → Tpt)
        traverse(tpt)

      case _ =>
        super.traverse(tree)
    }
    
    def visit(tree: Tree) = {
      
      def isTopLevelTree = {
        global.unitOfFile(tree.pos.source.file).body.pos == tree.pos
      }
      
      rootScope = if (isTopLevelTree)
        TreeScope(None, 0, tree.pos.source.length, tree.pos.source, 0, tree)
      else if (tree.pos.isRange)
        TreeScope(None, tree.pos.start, tree.pos.end, tree.pos.source, indentationLength(tree.pos.start, tree.pos.source), tree)
      else
        throw new Exception("Tree does not have a RangePosition: "+ tree)
      
      currentScope = rootScope
      
      traverse(tree)

      rootScope
    }
  }
  
  def essentialFragments(root: Tree, fs: FragmentRepository) = new Visitor {
    val handle = new BasicContribution with RequisitesContribution with ModifiersContribution with ScopeContribution with FragmentContribution {
       
      val global = self.global
      
      def getIndentation(start: Int, tree: Tree, scope: Scope): Int = {
        val self = indentationLength(start, tree.pos.source)
         
        val scopeIndentation = scope match {
          case scope: TreeScope => indentationLength(scope.start, scope.file)
          case _ => -1
        }
         
        if(scopeIndentation == self)
          return 0
         
        val outer = fs.scopeIndentation(tree).getOrElse(0)
        self - outer
      }
    }
  }.visit(root)
  
  
  def splitIntoFragments(root: Tree): TreeScope = new Visitor {
    val handle = new BasicContribution with ModifiersContribution with ScopeContribution with FragmentContribution {
      
      val global = self.global
      
      def getIndentation(start: Int, tree: Tree, scope: Scope) = { 
        val self = indentationLength(start, tree.pos.source)
        val outer = scope.indentation
        self - outer
      }
    }
  }.visit(root)
}
