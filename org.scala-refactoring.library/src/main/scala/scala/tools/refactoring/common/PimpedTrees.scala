/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.symtab.Flags
import tools.nsc.util.RangePosition
import tools.nsc.ast.parser.Tokens
import reflect.ClassManifest.fromClass
import tools.nsc.symtab.Flags
import scala.tools.nsc.Global
import util.Memoized

/**
 * A collection of implicit conversions for ASTs and other 
 * helper functions that work on trees.
 */
trait PimpedTrees {
  
  pimpedTrees: CompilerAccess =>
    
  import global._

  /**
   * Returns the tree that is contained in this file. Is
   * overridden in testing to manipulate the trees (i.e.
   * remove compiler generated trees)
   */
  def treeForFile(file: tools.nsc.io.AbstractFile): Option[Tree] = compilationUnitOfFile(file) map (_.body)
    
  /**
   * Returns the compilation unit root for that position.
   */  
  def cuRoot(p: Position): Option[Tree] = if (p == NoPosition) None else treeForFile(p.source.file)

  /**
   * Represent an import selector as a tree, including both names as trees.
   */
  case class ImportSelectorTree(name: NameTree, rename: global.Tree) extends global.Tree {
    def errorSubtrees = Nil
  }
  
  /**
   * Import selectors are not trees, but we can provide an extractor
   * that converts the ImportSelectors into our own ImportSelectorTrees.
   */
  class ImportSelectorTreeExtractor(t: global.Import) {
    // work around for https://lampsvn.epfl.ch/trac/scala/ticket/3392
    def Selectors(ss: List[global.ImportSelector] = t.selectors) = ss map { imp: global.ImportSelector =>
    
      val pos = {
        if(!t.pos.isDefined || imp.namePos  < 0)
          NoPosition
        else 
          new RangePosition(t.pos.source, imp.namePos, imp.namePos, imp.namePos + imp.name.length)
      }
    
      val name = NameTree(imp.name) setPos pos
      
      if(imp.renamePos < 0 || imp.name == imp.rename) {
        ImportSelectorTree(
          name, 
          global.EmptyTree) setPos name.pos
      } else {
        val newName = NameTree(imp.rename)
        val newTree = ImportSelectorTree(name, newName)
        
        if(t.pos.isRange) {
          newName setPos new RangePosition(t.pos.source, imp.renamePos, imp.renamePos, imp.renamePos + imp.rename.length) 
          newTree setPos (name.pos withPoint newName.pos.start withEnd newName.pos.end)
        }
        
        newTree
      }
    }
    
    object Selectors {
      def unapply(ss: List[global.ImportSelector]) = {
        Some(Selectors(ss))
      }
    }    
  }

  /**
   * Searches for a Symbol of a name in the type members of a tree.
   * 
   * This is mainly used for ImportSelectors, which don't carry any
   * symbol information with them.
   * 
   * @param expr The expr of an Import tree.
   * @param name The name of an ImportSelector of the import.
   */
  def findSymbolForImportSelector(expr: Tree, name: Name): Option[Symbol] = {
    val candidates = expr.tpe.members filter { sym =>
      name.toString == sym.name.toString
    }
    // There are sometimes multiple candidate symbols with the correct name; e.g. a class and an object symbol.
    // This picks one which is most useful for semantic highlighting:
    (candidates find { _.isCase }) orElse
      (candidates find { s => s.isClass || s.isTrait }) orElse
      candidates.headOption
  }
  
  implicit def importToImportSelectorTreeExtractor(t: global.Import) = new ImportSelectorTreeExtractor(t)
  
  /**
   * Add some methods to Tree that make it easier to compare
   * Trees by position and to extract the position of a tree's
   * name, which is tricky for Selects.
   */
  class TreeMethodsForPositions(t: Tree) {
    def hasExistingCode = t != null && !t.isEmpty && t.pos.isRange
    def hasNoCode = t != null && !t.isEmpty && t.pos == NoPosition
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && /*t.pos.point == p.point &&*/ t.pos.source == p.source && t.pos.isTransparent == p.isTransparent
    def samePos(o: Tree): Boolean = samePos(o.pos)
    def samePosAndType(o: Tree): Boolean = samePos(o.pos) && fromClass(o.getClass).equals(fromClass(t.getClass))
    
    /**
     * Returns the position this tree's name has in the source code.
     * Can also return NoPosition if the tree does not have a name.
     */
    def namePosition(): Position = {

      // TODO convert to an extractor, but there seems to be a strange problem with 2.10
      def findAllBinds(ts: List[Tree]): List[Tree] = {
        ts.collect {
          case b: Bind => List(b)
          case UnApply(_, args) => findAllBinds(args)
          case Apply(_, args) => findAllBinds(args)
          case _ => Nil
        }.flatten
      }
      
      def hasSingleBindWithTransparentPosition(ts: List[Tree]) = {
        findAllBinds(ts) match {
          case x :: Nil => x.pos.isTransparent
          case _ => false
        }
      }
      
      val pos = try {
        t match {
          case t if t.pos == NoPosition => NoPosition
          case t: ModuleDef => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
          case t: ClassDef  => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
          case t: TypeDef   => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
          case ValDef(_, _, _, Match(_, CaseDef(unapply: UnApply , _, _) :: Nil)) if hasSingleBindWithTransparentPosition(unapply.args) => 
            // modify the position to remove the transparency..
            val b = findAllBinds(unapply.args).head 
            b.pos withEnd b.namePosition.end
          case ValDef(_, _, _, Match(_, CaseDef(apply: Apply, _, _) :: Nil)) if hasSingleBindWithTransparentPosition(apply.args) =>
            val b = findAllBinds(apply.args).head
            b.pos withEnd b.namePosition.end
          case t: ValOrDefDef =>
            
            val name = t.symbol match {
              case NoSymbol => t.name.toString.trim
              case ts: TermSymbol if ts.isLazy => ts.lazyAccessor.nameString
              case _ => t.symbol.nameString
            }
            
            /* In general, the position of the name starts from t.pos.point and is as long as the trimmed name.
             * But if we have a val in a function: 
             *   ((parameter: Int) => ..)
             *     ^^^^^^^^^^^^^^
             * then the position of the name starts from t.pos.start. To fix this, we extract the source code and
             * check where the parameter actually starts.
             */
            lazy val src = t.pos.source.content.slice(t.pos.start, t.pos.point).mkString("")
            
            val pos = if(t.pos.point - t.pos.start == name.length && src == name) 
              t.pos withEnd t.pos.point
            else 
              new tools.nsc.util.RangePosition(t.pos.source, t.pos.point, t.pos.point, t.pos.point + name.length)

            if(t.mods.isSynthetic && t.pos.isTransparent) 
              pos.makeTransparent
            else
              pos
              
          case t @ Select(qualifier: New, selector) if selector.toString == "<init>" =>
            t.pos withEnd t.pos.start
            
          case t @ Select(qualifier, selector) => 
                      
            if (selector.decode startsWith "unary_") /* e.g. !true */ {
              t.pos withEnd (t.pos.start + nameString.length)
            } else if (t.pos.isRange && t.pos.source.content(t.pos.point) == '`') /*`a`*/ {
              t.pos withStart t.pos.point
            } else if (qualifier.pos.sameRange(t.pos) && t.name.toString == "apply") {
              t.pos withEnd t.pos.start
            } else if (qualifier.pos.isRange && t.symbol != NoSymbol) {
              t.pos withStart (t.pos.end - nameString.length)
            } else if (qualifier.pos.isRange && (t.pos.point.max(qualifier.pos.end + 1)) <= t.pos.end) {
              t.pos withStart (t.pos.point.max(qualifier.pos.end + 1))
            } else if (qualifier.pos == NoPosition) {
              t.pos
            /*the name contains symbols:*/
            } else if (t.name.decode != t.name.toString) {
              t.pos withEnd (t.pos.start + nameString.length)
            } else {
              t.pos withEnd (t.pos.start + t.name.length)
            }
            
          case t @ Bind(name, body) =>
            t.pos withEnd (t.pos.start + nameString.length)
          
          case t @ LabelDef(name, _, _) if name.toString startsWith "while" =>
            t.pos withEnd (t.pos.start + "while".length)
            
          case t @ LabelDef(name, _, Block(stats, cond)) if name.toString startsWith "doWhile" =>
            val src = stats.last.pos.source.content.slice(stats.last.pos.end, cond.pos.start) mkString
            val whileStart = stats.last.pos.end + src.indexOf("while")
            t.pos withStart whileStart withEnd (whileStart + "while".length)
            
          case t: SelectFromTypeTree =>
            t.pos withStart t.pos.point
            
          case t: DefTree => t.pos withStart t.pos.point withEnd (t.pos.point + nameString.length)
          
          case t => t.pos
        } 
      } catch {
        case _: java.lang.AssertionError => 
          /*we constructed an illegal position..*/
          NoPosition
      }
      
      pos match {
        case NoPosition => NoPosition
        case _ =>
          
          // it might be a quoted literal:
          val pos2 = {
            val src = pos.source.content
            if(pos.start >= 0 && pos.start < src.length && src(pos.start) == '`') {
              val startSearchForClosingTick = pos.start + 1
              val literalLength = src.slice(startSearchForClosingTick, src.length).takeWhile(_ != '`').length
              pos withEnd (pos.start + literalLength + "``".length)
            } else pos
          }
                        
          // set all points to the start, keeping wrong points
          // around leads to the calculation of wrong lines
          if(pos2.isTransparent)
            pos2 withPoint pos2.start makeTransparent
          else
            pos2 withPoint pos2.start
      }
    }

    /**
     * Returns the name for the tree that matches what was
     * printed in the source code. Compiler generated names
     * for '_' return '_' and otherwise synthetic names
     * return "". 
     */
    def nameString: String = {
      
      def extractName(name: Name) = {
        if(name.toString == "<empty>")
          ""
        else if (t.symbol.isSynthetic && name.toString.contains("$"))
          "_"
        else if (t.symbol.isSynthetic)
          ""
        else if (t.symbol != NoSymbol) {
          t.symbol.nameString
        } else 
          name.toString.trim
      }
        
      t match {
        case t: Select if t.name.toString endsWith "_$eq"=>
          val n = extractName(t.name)
          n.substring(0, n.length - "_=".length)
        case t: Select if t.name.toString startsWith "unary_"=>
          t.symbol.nameString.substring("unary_".length)
        case t: Select if t.symbol != NoSymbol =>
          t.symbol.nameString
        case t: LabelDef if t.name.toString startsWith "while" => "while"
        case t: LabelDef if t.name.toString startsWith "doWhile" => "while"
        case t: DefTree => extractName(t.name)
        case t: RefTree => extractName(t.name)
        case t: NameTree => t.nameString
        case t: TypeTree => t.symbol.nameString // FIXME: use something better
        case ImportSelectorTree(NameTree(name), _) => name.toString
        case _ => Predef.error("Tree "+ t.getClass.getSimpleName +" does not have a name.")
      }
    }
  }
  
  implicit def additionalTreeMethodsForPositions(t: Tree) = new TreeMethodsForPositions(t)
  
  /**
   * Finds a tree by its position, can be used to find
   * the original tree from a transformed tree.
   * 
   * If multiple trees are candidates, then take the last one, 
   * because it is likely more specific.
   */
  def findOriginalTree(tree: Tree) = {

    val candidates = findAllTreesWithTheSamePosition(tree)
    
    candidates find (_ eq tree) orElse (candidates filter (_ samePosAndType tree) lastOption)
  }
  
  val findAllTreesWithTheSamePosition: Tree => Iterable[Tree] = {
    Memoized.on ((t: Tree) => (t, t.pos)) { tree =>
  
      def find(t: Tree): List[Tree] = {
        (if(t samePos tree)
          t :: Nil
        else 
          Nil) ::: children(t).map(find).flatten
      }  
      
      cuRoot(tree.pos).map(find).toList.flatten
    }
  }

  class TemplateMethods(t: Template) {
    
    /**
     * Returns all constructor parameters from the template body.
     */
    def constructorParameters = t.body.collect {
      case vd @ ValDef(mods, _, _, _) if (mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) ) => vd
    }
    
    /**
     * Returns the primary constructor of the template.
     */
    def primaryConstructor = t.body.filter {
      case t: DefDef => 
        t.symbol.isPrimaryConstructor || (t.symbol == NoSymbol && t.name.toString == nme.CONSTRUCTOR.toString)
      case _ => false
    }
       
    /**
     * Returns all early definitions from the template body.
     */
    def earlyDefs = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats filter treeInfo.isEarlyDef
      case t @ DefDef(_, _, _, _, _, rhs) if t.symbol.isConstructor && treeInfo.isEarlyDef(rhs) => rhs :: Nil
    } flatten
      
    /**
     * Returns the trees that are passed to a super constructor call.
     */
    def superConstructorParameters = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor || t.name.toString == nme.CONSTRUCTOR.toString => stats collect {
        case Apply(_, args) => args
      } flatten
    } flatten
  }
  
  implicit def additionalTemplateMethods(t: Template) = new TemplateMethods(t)
  
  /**
   * Provides a finer-grained extractor for Template that distinguishes
   * between class constructor parameters, early definitions, parents, 
   * self type annotation and the real body.
   */
  object TemplateExtractor {
    def unapply(t: Tree) = t match {
      case tpl: Template => 
      
        val pimpedTpl = additionalTemplateMethods(tpl)
              
        val primaryConstructorArgs = (pimpedTpl.primaryConstructor flatMap (t => t.asInstanceOf[DefDef].vparamss)) map (_.size)
        
        // FIXME this is very very ugly
        val classParams: List[List[ValDef]] = {
          
          var cp = pimpedTpl.constructorParameters
        
          if(primaryConstructorArgs.sum != cp.size) {
            List(cp)
          } else {
          
            val xx = primaryConstructorArgs map { i =>
              val(current, rest) = cp.splitAt(i)
              cp = rest
              current
            }
            
            assert(cp == Nil)
            
            xx
          }
        }
        
        val body = {
          val bodyWithoutPrimaryConstructorAndArgs = tpl.body filterNot (pimpedTpl.primaryConstructor ::: pimpedTpl.constructorParameters contains) 
          val removeGeneratedTrees = bodyWithoutPrimaryConstructorAndArgs filter keepTree
          removeCompilerTreesForMultipleAssignment(removeGeneratedTrees)
        }
        
        val parents = (pimpedTpl.superConstructorParameters match {
          case Nil => tpl.parents
          case params => SuperConstructorCall(tpl.parents.head, params) :: tpl.parents.tail
        }) filterNot (_.isEmpty) filter {
          // objects are always serializable, but the Serializable parent's position is NoPosition
          case t: TypeTree if t.pos == NoPosition && t.nameString == "Serializable" => false
          case _ => true
        }
        
        val self = if(tpl.self.isEmpty) EmptyTree else {
          
          if(tpl.pos.isRange) {
            val source = tpl.self.pos.source.content.slice(tpl.self.pos.point, tpl.self.pos.end) mkString // XXX remove comments
            
            def extractExactPositionsOfAllTypes(typ: Type): List[Tree] = typ match {
              case RefinedType(parents, _) =>
                parents flatMap extractExactPositionsOfAllTypes
              case tpe @ TypeRef(_, sym, _) =>
                val thisName = typ.toString
                val nameIndex = source.indexOf(thisName)
                if(nameIndex < 0) 
                  Nil
                else {
                  val start = tpl.self.pos.point + nameIndex
                  val end = start + thisName.length
                  
                  List(TypeTree(tpe) setPos (tpl.self.pos withStart start withEnd end))
                }
              case _ => Nil
            }
            
            val selfTypes = extractExactPositionsOfAllTypes(tpl.self.tpt.tpe)
            
            val selfNameTree = if(tpl.self.name.toString == "_") {
              NameTree("this") setPos (tpl.self.pos withEnd (tpl.self.pos.start + "this".length))
            } else {
              NameTree(tpl.self.name) setPos {
                val p = tpl.self.pos
                p withEnd (if(p.start == p.point) p.end else p.point)
              }
            }
            
            SelfTypeTree(selfNameTree, selfTypes, tpl.self.tpt) setPos tpl.self.pos
          } else {
            tpl.self
          }
        }

        Some((classParams, pimpedTpl.earlyDefs, parents, self, body))
      
      case _ => 
        None
    }
  }
  
  
  /**
   * Returns all children that have a representation in the source code.
   * This includes Name and Modifier trees and excludes everything that
   * has no Position or is an EmptyTree.
   */
  def children(t: Tree): List[Tree] = (t match {
    
    case PackageDef(pid, stats) => 
      pid :: stats
    
    case t @ ClassDef(ModifierTree(mods), name, tparams, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: impl :: Nil
      
    case t @ ModuleDef(ModifierTree(mods), name, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: impl :: Nil
      
    case TemplateExtractor(params, earlyBody, parents, self, body) =>
      params.flatten ::: earlyBody ::: parents ::: self :: removeCompilerTreesForMultipleAssignment(body)

    case t @ ValDef(ModifierTree(mods), name, tpt, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tpt :: rhs :: Nil
     
    case t @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: vparamss.flatten ::: tpt :: rhs :: Nil
     
    case t: TypeTree =>
      if(t.original != null) t.original :: Nil else Nil
      
    case AppliedTypeTree(tpt, args) =>
      tpt :: args
      
    case TypeDef(ModifierTree(mods), name, tparams, rhs) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: rhs :: Nil
      
    case Bind(name, body) =>
      (NameTree(name) setPos t.namePosition) :: body :: Nil
    
    case _: Literal | _: Ident | _: ModifierTree | _: NameTree | _: This | _: Super => Nil
    
    case Apply(fun, args) =>
      fun :: args
       
    case t @ Select(qualifier, selector) if selector.toString.startsWith("unary_")=>
      (NameTree(t.nameString) setPos t.namePosition) :: qualifier :: Nil
      
    case t @ Select(qualifier: This, selector) if qualifier.pos == NoPosition && t.pos.isRange && t.pos.start == t.pos.point =>
      (NameTree(selector) setPos t.namePosition) :: Nil
      
    case t @ Select(qualifier, selector) =>
      qualifier :: (NameTree(selector) setPos t.namePosition) :: Nil
      
    case BlockExtractor(stats) =>
      stats
      
    case Return(expr) =>
      expr :: Nil
      
    case New(tpt) =>
      tpt :: Nil
      
    case Match(selector, cases) =>
      selector :: cases
      
    case CaseDef(pat, guard, body) =>
      pat :: guard :: body :: Nil
      
    case t @ Import(expr, _) =>
      expr :: t.Selectors()
      
    case ImportSelectorTree(name, rename) =>
      name :: rename :: Nil
      
    case SuperConstructorCall(clazz, args) =>
      clazz :: args
      
    case SelfTypeTree(name, types, orig) =>
      name :: types ::: orig :: Nil
      
    case TypeApply(fun, args) =>
      fun :: args
      
    case Function(vparams, body) =>
      vparams ::: body :: Nil
      
    case If(cond, thenp, elsep) =>
      cond :: thenp :: elsep :: Nil
      
    case TypeBoundsTree(lo, hi) =>
      lo :: hi :: Nil
      
    case Typed(expr, tpt) =>
      expr :: tpt :: Nil
      
    case Assign(lhs, rhs) =>
      lhs :: rhs :: Nil
      
    case Alternative(trees) =>
      trees
      
    case UnApply(fun, args) =>
      fun :: args
      
    case Star(elem) =>
      elem :: Nil
      
    case Try(block, catches, finalizer) =>
      block :: catches ::: finalizer  :: Nil
    
    case Throw(expr) =>
      expr :: Nil
      
    case Annotated(annot, arg) =>
      annot :: arg :: Nil
      
    case CompoundTypeTree(templ) =>
      templ :: Nil
      
    // while loop  
    case LabelDef(name, params, If(cond, then, _)) =>
      (NameTree(name) setPos t.namePosition) :: params ::: cond :: then :: Nil
      
    // do .. while loop  
    case LabelDef(name, params, Block(stats, If(cond, _, _))) =>
      stats ::: (NameTree(name) setPos t.namePosition) :: cond :: Nil
   
    case ExistentialTypeTree(tpt, whereClauses) =>
      tpt :: whereClauses
      
    case t @ SelectFromTypeTree(qualifier, _) =>
      qualifier :: (NameTree(t.nameString) setPos t.namePosition) :: Nil
      
    case SingletonTypeTree(ref) =>
      ref :: Nil
      
    case AssignOrNamedArg(lhs, rhs) =>
      lhs :: rhs :: Nil
      
    case MultipleAssignment(extractor, values, rhs) =>
      extractor :: values ::: rhs :: Nil
      
    case DocDef(_, definition) =>
      definition :: Nil
      
    case _ => Nil
     
  }) map {
                 
     /**
     * An empty RHS that is implemented as '.. { }' creates a Literal 
     * tree with a range length of 1, remove that tree.
     */
    case t: Literal if t.pos.isRange && t.pos.end - t.pos.start == 1 && t.toString == "()" => 
      EmptyTree
      
    /**
     * hide the implicit "apply" call
     */
    case t @ Select(qualifier: Select, name) if name.toString == "apply" && t.samePos(qualifier) => 
      qualifier
          
    case t => t
  } filter keepTree
  
  private def removeCompilerTreesForMultipleAssignment(body: List[Tree]): List[Tree] = {
    
    def mkMultipleAssignment(extractor: Tree, tpe: Type, pos: Position, rhs: Tree, trailingTrees: List[Tree]) = {
      val numberOfAssignments = tpe match {
        case tpe: TypeRef => tpe.args.size
        case _ => 0
      }
      
      val (values, rest) = trailingTrees splitAt numberOfAssignments
      
      val valDefs = values collect {case v: ValDef => v copy (mods = Modifiers(Flags.SYNTHETIC)) setPos v.pos}
              
      MultipleAssignment(extractor, valDefs, rhs).setPos(pos) :: removeCompilerTreesForMultipleAssignment(rest)
    }
    
    body match {
       case (v @ ValDef(_, _, _, Match(rhs: Typed, (c @ CaseDef(_: Apply, EmptyTree, body)) :: Nil))) :: xs 
          if v.symbol.isSynthetic && (c.pos.isTransparent || c.pos == NoPosition) =>
        mkMultipleAssignment(EmptyTree, body.tpe, v.pos, rhs, xs)
      
      case (v @ ValDef(_, _, _, Match(rhs: Typed, (c @ CaseDef(extractor: UnApply, EmptyTree, body)) :: Nil))) :: xs 
          if v.symbol.isSynthetic && (c.pos.isTransparent || c.pos == NoPosition) =>
        mkMultipleAssignment(extractor, body.tpe, v.pos, rhs, xs)
        
      case x :: xs => x :: removeCompilerTreesForMultipleAssignment(xs)
      case x => x
    }
  }
  
  val originalParentOf: Tree => Option[Tree] = Memoized { tree => 
    cuRoot(tree.pos) flatMap { root =>
    
      def find(parent: Tree): Option[Tree] = {
        val cs = children(parent)
        
        if(cs.exists(_ samePosAndType tree))
          Some(parent)
        else
          cs.flatMap(find).lastOption
      }
      
      find(root)
    }
  }
  
  val (originalLeftSibling, originalRightSibling) = {
    
    def findSibling(t: Tree, parent: Option[Tree], compareIndex: Int, returnIndex: Int) = parent flatMap 
      (children(_) filter (_.pos.isRange) sliding 2 find (_ lift compareIndex map (_ samePos t) getOrElse false) flatMap (_ lift returnIndex))
   
    ((t: Tree) => findSibling(t, originalParentOf(t), 1, 0)) → ((t: Tree) => findSibling(t, originalParentOf(t), 0, 1))
  }
  
  def keepTree(t: Tree) = !t.isEmpty && (t.pos.isRange || t.pos == NoPosition)
  
  /**
   * Represent a Name as a tree, including its position.
   */
  case class NameTree(name: global.Name) extends global.Tree {
    if (name.toString == "<none>") Predef.error("Name cannot be <none>, NoSymbol used?")
    def nameString = {
      if(pos.isRange && pos.source.content(pos.start) == '`' && !name.toString.startsWith("`")) {
        "`"+ name.toString.trim +"`"
      } else {
        name.toString.trim 
      }
    }
    override def toString = "NameTree("+ nameString +")"
    override def setPos(p: Position) = {
      if(p != NoPosition && p.start < 0) {
        Predef.error("pos.start is"+ p.start)
      }
      super.setPos(p)
    }
    def errorSubtrees = Nil
  }
  
  /**
   * Represent a modifier as a tree, including its position.
   */
  case class ModifierTree(flag: Long) extends global.Tree {
    
    override def toString = "ModifierTree("+ nameString +")"
    
    import Flags._
    
    def nameString = flag match {
      case 0            => ""
      case TRAIT        => "trait"
      case METHOD       => "def"
      case FINAL        => "final"
      case IMPLICIT     => "implicit"
      case PRIVATE      => "private"
      case PROTECTED    => "protected"
      case SEALED       => "sealed"
      case OVERRIDE     => "override"
      case CASE         => "case"
      case ABSTRACT     => "abstract"
      case PARAM        => ""
      case LAZY         => "lazy"
      case Tokens.VAL   => "val"
      case Tokens.VAR   => "var"
      case Tokens.TYPE  => "type"
      case Tokens.DEF   => "def"
      case _            => "<unknown>: " + flagsToString(flag)
    }
    
    def errorSubtrees = Nil
  } 
    
  /**
   * Extract the modifiers with their position from a Modifiers
   * object.
   */
  object ModifierTree {
    def unapply(m: global.Modifiers) = {
      Some(m.positions.toList map {
        case (flag, pos) if pos.isRange =>
          ModifierTree(flag) setPos (pos withEnd (pos.end + 1))
        case (flag, _) =>
          ModifierTree(flag)
      })
    }
  }
  
  /**
   * The call to the super constructor in a class:
   * class A(i: Int) extends B(i)
   *                         ^^^^ 
   */
  case class SuperConstructorCall(clazz: global.Tree, args: List[global.Tree]) extends global.Tree {
    if(clazz.pos != global.NoPosition) setPos(clazz.pos withEnd args.lastOption.getOrElse(clazz).pos.end)

    def errorSubtrees = Nil
  }
  
  /**
   * Representation of self type annotations:
   *   self: A with B =>
   *   ^^^^^^^^^^^^^^
   */
  case class SelfTypeTree(name: NameTree, types: List[global.Tree], orig: Tree) extends global.Tree {
    def errorSubtrees = Nil
  }
    
  /**
   * Unify the children of a Block tree and sort them 
   * in the same order they appear in the source code.
   * 
   * Also reshapes some trees: multiple assignments are
   * removed and named arguments are reordered.
   */
  object BlockExtractor {
    
    def unapply(t: Block) = {

      def fixNamedArgumentCall(block: Block): Tree = block match {
        case Block(stats, apply @ Apply(fun: Select, emptyArgs)) if apply.pos.isRange && emptyArgs.size == stats.size && emptyArgs.forall(i => i.isEmpty || !i.pos.isRange) =>
        
          val allValDefs = stats forall {
            case t: ValDef => t.pos.isRange && t.pos.start > apply.pos.start
            case _ => return block
          }
          
          val argumentNames = fun.tpe match {
            case tpe: MethodType => tpe.params map (_.name)
            case _ => return block
          }
          
          // The arguments of apply all have an offset position, so they
          // were removed during the transformations. Therefore we have
          // to look up the original apply method
          val argumentsFromOriginalTree = compilationUnitOfFile(apply.pos.source.file) map (_.body) flatMap { root =>
            root.find(_ samePos apply) collect { case Apply(_, args) => args }
          } getOrElse (return block)
          
          val syntheticNamesToRealNames = (argumentsFromOriginalTree map { 
            case a: Ident => a.name 
            case _ => return block
          }) zip argumentNames toMap
          
          val startOffset = apply.pos.point 
          // FIXME strip comments!
          val argumentsSource = apply.pos.source.content.slice(startOffset, apply.pos.end) mkString
          
          val newValDefs = stats collect {
            case t: ValDef if t.pos != NoPosition=> 
              val newVal = t.copy(name = syntheticNamesToRealNames(t.name)) 
              // FIXME we can do a better search..
              val nameStart = argumentsSource.indexOf(newVal.name.toString)
              
              if(nameStart >= 0) {
                val nameLength = newVal.name.length
                newVal setPos (t.pos withStart (nameStart + startOffset) withPoint nameStart + startOffset + nameLength)
              } else /*no named argument*/ {
                t.rhs
              }
          }
          
          Apply(fun, newValDefs) setPos apply.pos
            
        case _ => block
      }
      
      fixNamedArgumentCall(t) match {
        case t: Block => 
       
          val trees = if(t.expr.pos.isRange && t.stats.size > 0 && (t.expr.pos precedes t.stats.head.pos))
            t.expr :: t.stats
          else
            t.stats ::: t.expr :: Nil
            
          val fixedTrees = removeCompilerTreesForMultipleAssignment(trees)
    
          Some(fixedTrees filter keepTree)       
          
        case t => Some(List(t))
      }
    }
  }
  
  case class MultipleAssignment(extractor: Tree, names: List[ValDef], rhs: Tree) extends global.Tree {
    def errorSubtrees = Nil
  }
   
  class NotInstanceOf[T](m: Manifest[T]) {
    def unapply(t: Tree): Option[Tree] = {
      if(m.erasure.isInstance(t)) {
        None
      } else
        Some(t)
    }
  }                                              
  
  object NotInstanceOf {
    def apply[T](implicit m: Manifest[T]) = {
      new NotInstanceOf[T](m)
    }
  }
  
  val NoBlock = NotInstanceOf[Block]
  val NoPackageDef = NotInstanceOf[PackageDef]
 
  /**
   * A SourceLayoutTree can be used to insert arbitrary text into the code,
   * for example, blank lines.
   */
  case class SourceLayoutTree(kind: SourceLayouts.Kinds) extends global.Tree {
    def errorSubtrees = Nil
  }
  object SourceLayouts {
    sealed trait Kinds
    object Newline extends Kinds
  }
}
