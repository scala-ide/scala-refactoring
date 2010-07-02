/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.symtab.Flags
import tools.nsc.util.RangePosition
import tools.nsc.ast.parser.Tokens
import reflect.ClassManifest.fromClass
import tools.nsc.io.AbstractFile
import tools.nsc.symtab.{Flags, Names}

/**
 * A bunch of implicit conversions for ASTs and other helper
 * functions that work on trees. Users of the trait need to
 * provide the means to access a file's corresponding tree.
 * 
 * */
trait PimpedTrees {
    
  val global: scala.tools.nsc.interactive.Global
  import global._

  /**
   * Returns the tree that is contained in this file. Is
   * overriden in testing to manipulate the trees (i.e.
   * remove compiler generated trees)
   * */
  def treeForFile(file: AbstractFile): Option[Tree] = unitOfFile.get(file) map (_.body)
    
  /**
   * Returns the compilation unit root for that position.
   * */  
  def cuRoot(p: Position): Option[Tree] = if (p == NoPosition) None else treeForFile(p.source.file)

  
  /**
   * Represent an import selector as a tree, including both names as trees.
   * */
  case class ImportSelectorTree(name: NameTree, rename: global.Tree) extends global.Tree
  
  /**
   * Import selectors are not trees, but we can provide an extractor
   * that converts the ImportSelectors into our own ImportSelectorTrees.
   * */
  implicit def importToImportSelectorTreeExtractor(t: global.Import) = new {
    // work around for https://lampsvn.epfl.ch/trac/scala/ticket/3392
    def Selectors(ss: List[global.ImportSelector] = t.selectors) = ss map { imp: global.ImportSelector =>
    
      val name = NameTree(imp.name) setPos new RangePosition(t.pos.source, imp.namePos, imp.namePos, imp.namePos + imp.name.length)
      
      if(imp.renamePos < 0 || imp.name == imp.rename) {
        ImportSelectorTree(
          name, 
          global.EmptyTree) setPos name.pos
      } else {
        val rename = NameTree(imp.rename) setPos new RangePosition(t.pos.source, imp.renamePos, imp.renamePos, imp.renamePos + imp.rename.length) 
        ImportSelectorTree(
          name, 
          rename) setPos (name.pos withPoint rename.pos.start withEnd rename.pos.end)
      }
    }
    
    object Selectors {
      def unapply(ss: List[global.ImportSelector]) = {
        Some(Selectors(ss))
      }
    }
  }
  
  /**
   * Add some methods to Tree that make it easier to compare
   * Trees by position and to extract the position of a tree's
   * name, which is tricky for Selects.
   * */
  implicit def additionalTreeMethodsForPositions(t: Tree) = new {
    def hasExistingCode = t != null && !t.isEmpty && t.pos.isRange
    def hasNoCode = t != null && !t.isEmpty && t.pos == NoPosition
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && t.pos.source == p.source && t.pos.isTransparent == p.isTransparent
    def samePos(o: Tree)    : Boolean = samePos(o.pos)
    def sameTree(o: Tree)   : Boolean = samePos(o.pos) && fromClass(o.getClass).equals(fromClass(t.getClass))
    def namePosition(): Position = (t match {
      case t: ModuleDef   => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t: ClassDef    => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t: TypeDef    => t.pos withStart t.pos.point withEnd (t.pos.point + t.name.toString.trim.length)
      case t if t.pos == NoPosition => NoPosition
      case t: ValOrDefDef =>
        
        val name = if(t.symbol != NoSymbol) t.symbol.nameString else t.name.toString.trim
        
        /* In general, the position of the name starts from t.pos.point and is as long as the trimmed name.
         * But if we have a val in a function: 
         *   ((parameter: Int) => ..)
         *     ^^^^^^^^^^^^^^
         * then the position of the name starts from t.pos.start. To fix this, we extract the source code and
         * check where the parameter actually starts.
         * */
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
      
        if (qualifier.pos.isRange && qualifier.pos.start > t.pos.start && qualifier.pos.start >= t.pos.end) /* e.g. !true */ {
          t.pos withEnd (t.pos.start + nameString.length)
        } else if (t.pos.isRange && t.pos.source.content(t.pos.point) == '`') {
          t.pos withStart t.pos.point
        } else if (qualifier.pos.isRange && t.symbol != NoSymbol) {
          t.pos withStart (t.pos.end - nameString.length)
        } else if (qualifier.pos.isRange) {
          t.pos withStart (t.pos.point.max(qualifier.pos.end + 1))
        } else if (qualifier.pos == NoPosition) {
          t.pos
        } else {
          t.pos withEnd (t.pos.start + nameString.length)
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
        
      case _ => throw new Exception("uhoh")
    }) match {
      case NoPosition => NoPosition
      case p =>
        // it might be a quoted literal:
        val p2 = if(p.source.content(p.start) == '`') {
          p withEnd (p.end + 2) 
        } else p
      
        // set all points to the start, keeping wrong points
        // around leads to the calculation of wrong lines
        if(p2.isTransparent)
          p2 withPoint p2.start makeTransparent
        else
          p2 withPoint p2.start
    }
    
    private def extractName(name: Name) = 
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
    
    def nameString: String = t match {
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
      case ImportSelectorTree(NameTree(name), _) => name.toString
      case _ => Predef.error("Tree "+ t.getClass.getSimpleName +" does not have a name.")
    }
  }

  /**
   * Find a tree by its position and make sure that the trees
   * or of the same type. This is necessary because some trees
   * have the same position, for example, a compilation unit
   * without an explicit package and just a single top level
   * class, then the package and the class will have the same
   * position.
   * 
   * If multiple trees are candidates, then take the last one, 
   * because it is likely more specific.
   * */
  def findOriginalTree(t: Tree): Option[Tree] = {
    
    def find(t: Tree): List[Tree] = {
      (if(t samePos t.pos)
        t :: Nil
      else 
        Nil) ::: children(t).map(find).flatten
    }
      
    val candidates = cuRoot(t.pos) map find flatten
    
    candidates find (_ == t) match {
      case None => candidates filter (_ sameTree t) lastOption
      case Some(perfectMatch) => Some(perfectMatch)
    }
  }
  
  implicit def additionalTemplateMethods(t: Template) = new {
    def constructorParameters = t.body.filter {
      case ValDef(mods, _, _, _) => mods.hasFlag(Flags.CASEACCESSOR) || mods.hasFlag(Flags.PARAMACCESSOR) 
      case _ => false
    }
    
    def primaryConstructor = t.body.filter {
      case t: DefDef => t.symbol.isPrimaryConstructor
      case _ => false
    }
    
    def earlyDefs = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats filter treeInfo.isEarlyDef
      case t @ DefDef(_, _, _, _, _, rhs)        if t.symbol.isConstructor && treeInfo.isEarlyDef(rhs) => rhs :: Nil
    } flatten
    
    def superConstructorParameters = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats collect {
        case Apply(Super(_, _), args) => args
      } flatten
    } flatten
  }  
  
  /**
   * Name objects are not trees, this extractor creates NameTree instances from Trees.
   * */
  implicit def nameTreeToNameTreeExtractor(t: global.Tree) = new {
    object Name {
      def unapply(name: global.Name) = {
        Some(NameTree(name) setPos t.namePosition)
      }
    }
  }
  
  /**
   * Provides a finer-grained extractor for Template that distinguishes
   * between class constructor parameters, early definitions, parents, 
   * self type annotation and the real body.
   * */
  object TemplateExtractor {
    def unapply(t: Tree) = t match {
      case tpl: Template => 
      
        val pimpedTpl = additionalTemplateMethods(tpl)
              
        val classParams = pimpedTpl.constructorParameters
        
        val body = removeCompilerTreesForMultipleAssignment((tpl.body filterNot (pimpedTpl.primaryConstructor ::: classParams contains)) filter keepTree)
        
        val parents = (pimpedTpl.superConstructorParameters match {
          case Nil => tpl.parents
          case params => SuperConstructorCall(tpl.parents.head, params) :: tpl.parents.tail
        }) filterNot (_.isEmpty)
        
        val self = if(tpl.self.isEmpty) EmptyTree else {
          
          if(tpl.pos.isRange) {
            val source = tpl.self.pos.source.content.slice(tpl.self.pos.point, tpl.self.pos.end) mkString // XXX remove comments
            
            def extractExactPositionsOfAllTypes(typ: Type): List[NameTree] = typ match {
              case RefinedType(parents, _) =>
                parents flatMap extractExactPositionsOfAllTypes
              case TypeRef(_, sym, _) =>
                val thisName = sym.name.toString
                val nameIndex = source.indexOf(thisName)
                if(nameIndex < 0) 
                  Nil
                else {
                  val start = tpl.self.pos.point + nameIndex
                  val end = start + thisName.length
                  List(NameTree(sym.name) setPos (tpl.self.pos withStart start withEnd end))
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
   * */
  def children(t: Tree): List[Tree] = (t match {
    
    case PackageDef(pid, stats) => 
      pid :: stats
    
    case t @ ClassDef(ModifierTree(mods), name, tparams, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: impl :: Nil
      
    case t @ ModuleDef(ModifierTree(mods), name, impl) =>
      mods ::: (NameTree(name) setPos t.namePosition) :: impl :: Nil
      
    case TemplateExtractor(params, earlyBody, parents, self, body) =>
      params ::: earlyBody ::: parents ::: self :: removeCompilerTreesForMultipleAssignment(body)

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
      
    case MultipleAssignment(values, rhs) =>
      values ::: rhs :: Nil
      
    case DocDef(_, definition) =>
      definition :: Nil
      
    case _ => throw new Exception("Unhandled tree: "+ t.getClass.getSimpleName)
     
  }) filter keepTree
  
  private[this] def removeCompilerTreesForMultipleAssignment(body: List[Tree]): List[Tree] = {
    body match {
      case (v @ ValDef(_, _, _, Match(rhs: Typed, c @ CaseDef(_: Apply, EmptyTree, body) :: Nil))) :: xs 
          if v.symbol.isSynthetic && c.forall(_.pos.isTransparent) =>
      
        val numberOfAssignments = body.tpe match {case tpe: TypeRef => tpe.args.size case _ => 0}
        
        val (values, rest) = xs splitAt numberOfAssignments
        
        val valDefs = values collect {case v: ValDef => v copy (mods = NoMods) setPos v.pos}
                
        MultipleAssignment(valDefs, rhs).setPos(v.pos) :: removeCompilerTreesForMultipleAssignment(rest)
        
      case x :: xs => x :: removeCompilerTreesForMultipleAssignment(xs)
      case x => x
    }
  }
  
  implicit def additionalValMethods(t: ValDef) = new {
    def needsKeyword =
      !t.mods.hasFlag(Flags.PARAM) &&
      !t.mods.hasFlag(Flags.PARAMACCESSOR) &&
      !t.mods.hasFlag(Flags.CASEACCESSOR) &&
      !t.mods.hasFlag(Flags.SYNTHETIC) &&
      !t.symbol.isSynthetic
  }
  
  /**
   * Make a Tree aware of its parent and siblings. Note
   * that these are expensive operations because they
   * traverse the whole compilation unit.
   * */
  implicit def additionalTreeMethodsForFamily(t: Tree) = new {
    def originalParent = cuRoot(t.pos) flatMap { root =>
    
      def find(root: Tree): Option[Tree] = {
        val cs = children(root)
        
        if(cs.exists(_ sameTree t))
          Some(root)
        else
          cs.flatMap(find).lastOption
      }
      find(root)
    }
    
    def originalLeftSibling  = findSibling(originalParent, 1, 0)
    
    def originalRightSibling = findSibling(originalParent, 0, 1)
    
    private def findSibling(parent: Option[Tree], compareIndex: Int, returnIndex: Int) = parent flatMap 
      (children(_) filter (_.pos.isRange) sliding 2 find (_ lift compareIndex map (_ samePos t) getOrElse false) flatMap (_ lift returnIndex))
  }
  
  implicit def additionalTreeListMethods(ts: List[Tree]) = new {
    def allOnSameLine: Boolean = {
      val poss = ts map (_.pos)
      poss.forall(_.isRange) && (poss.map(_.line).distinct.length <= 1)
    }
  }
  
  def keepTree(t: Tree) = !t.isEmpty && (t.pos.isRange || t.pos == NoPosition)
  
  /**
   * Represent a Name as a tree, including its position.
   * */
  case class NameTree(name: global.Name) extends global.Tree {
    if (name.toString == "<none>") Predef.error("Name cannot be <none>, NoSymbol used?")
    def nameString = {
      if(pos.isRange && pos.source.content(pos.start) == '`') {
        "`"+ name.toString.trim +"`"
      } else {
        name.toString.trim 
      }
    }
    override def toString = "NameTree("+ nameString +")"
    override def hashCode = {
      nameString.hashCode * 31 + (if(pos == NoPosition) NoPosition.hashCode else pos.start)
    }
    override def equals(other: Any) = other match {
      case other: NameTree if pos == NoPosition && other.pos == NoPosition => 
        other.nameString == nameString
      case other: NameTree if pos != NoPosition && other.pos != NoPosition => 
        other.nameString == nameString && other.pos.start == pos.start && other.pos.source == pos.source
      case _ => false
    }
  }
  
  /**
   * Represent a modifier as a tree, including its position.
   * */
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
  } 
    
  /**
   * Extract the modifiers with their position from a Modifiers
   * object.
   * */
  object ModifierTree {
    def unapply(m: global.Modifiers) = {
      Some(m.positions.toList map {
        case (flag, global.NoPosition) => 
          ModifierTree(flag)
        case (flag, pos) =>
          ModifierTree(flag) setPos (pos withEnd (pos.end + 1))
      })
    }
  }
  
  /**
   * The call to the super constructor in a class:
   * class A(i: Int) extends B(i)
   *                         ^^^^ 
   * */
  case class SuperConstructorCall(clazz: global.Tree, args: List[global.Tree]) extends global.Tree {
    if(clazz.pos != global.NoPosition) setPos(clazz.pos withEnd args.lastOption.getOrElse(clazz).pos.end)
  }
  
  /**
   * Representation of self type annotations:
   *   self: A with B =>
   *   ^^^^^^^^^^^^^^
   * */
  case class SelfTypeTree(name: NameTree, types: List[global.Tree], orig: Tree) extends global.Tree
    
  /**
   * Unify the children of a Block tree and sort them 
   * in the same order they appear in the source code.
   * 
   * Also reshapes some trees: multiple assignments are
   * removed and named arguments are reordered.
   * */
  object BlockExtractor {
    def unapply(t: Block) = {
      
      /**
       * Names argument calls are 
       * */
      def fixNamedArgumentCall(block: Block): Tree = block match {
        case Block(stats, apply @ Apply(fun: Select, emptyArgs)) if emptyArgs.size == stats.size && emptyArgs.forall(_.isEmpty) =>
        
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
          val argumentsFromOriginalTree = unitOfFile get apply.pos.source.file map (_.body) flatMap { root =>
            root.find(_ sameTree apply) collect { case Apply(_, args) => args }
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
  
  case class MultipleAssignment(names: List[ValDef], rhs: Tree) extends global.Tree
}