/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.symtab.Flags
import tools.nsc.ast.parser.Tokens
import tools.nsc.symtab.Flags
import scala.tools.nsc.Global
import util.Memoized
import scala.collection.mutable.ListBuffer
import scala.tools.refactoring.sourcegen.Fragment
import scala.tools.refactoring.sourcegen.AbstractPrinter
import scala.tools.refactoring.sourcegen.Layout
import scala.reflect.internal.util.RangePosition

import language.implicitConversions

/**
 * A collection of implicit conversions for ASTs and other
 * helper functions that work on trees.
 *
 * @TODO try to make this trait smaller by splitting it into smaller parts
 */
trait PimpedTrees {

  pimpedTrees: CompilerAccess =>

  import global._

  /**
   * Represent an import selector as a tree, including both names as trees.
   */
  case class ImportSelectorTree(name: pimpedTrees.NameTree, rename: global.Tree) extends global.Tree

  /**
   * Import selectors are not trees, but we can provide an extractor
   * that converts the ImportSelectors into our own ImportSelectorTrees.
   */
  implicit class ImportSelectorTreeExtractor(t: global.Import) {

    def Selectors(ss: List[global.ImportSelector] = t.selectors) = ss map { imp: global.ImportSelector =>

      val pos = {
        if(!t.pos.isDefined || imp.namePos  < 0)
          NoPosition
        else
          new RangePosition(t.pos.source, imp.namePos, imp.namePos, imp.namePos + imp.name.length)
      }

      assert(imp.name != nme.NO_NAME, "Unexpected name %s in %s. The full import tree is %s".format(imp.name, imp, t))

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
   * Takes a name and wraps it in `` if the name corresponds to a Scala keyword.
   */
  def escapeScalaKeywordsForImport(n: Name) = {
    if(global.nme.keywords.contains(n.toTermName) && n != nme.USCOREkw) "`"+ n.toString +"`" else n.toString
  }

  def escapeScalaKeywordsForImport(n: String) = {
    val name = newTermName(n)
    if(global.nme.keywords.contains(name) && name != nme.USCOREkw) "`"+ n +"`" else n
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
    val candidates = Option(expr.tpe).toList flatMap (_.members) filter { sym =>
      name.toString == sym.name.toString
    }
    // There are sometimes multiple candidate symbols with the correct name; e.g. a class and an object symbol.
    // This picks one which is most useful for semantic highlighting:
    (candidates find { _.isCase }) orElse
      (candidates find { s => s.isClass || s.isTrait }) orElse
      candidates.headOption
  }

  /**
   * Add some methods to Tree that make it easier to compare
   * Trees by position and to extract the position of a tree's
   * name, which is tricky for Selects.
   */
  class TreeMethodsForPositions(t: Tree) {
    def hasExistingCode = t != null && !isEmptyTree(t) && t.pos.isRange
    def hasNoCode = t != null && !isEmptyTree(t) && t.pos == NoPosition
    def samePos(p: Position): Boolean = t.pos.sameRange(p) && /*t.pos.point == p.point &&*/ t.pos.source == p.source && t.pos.isTransparent == p.isTransparent
    def samePos(o: Tree): Boolean = samePos(o.pos)
    def samePosAndType(o: Tree): Boolean = {
      samePos(o.pos) && (o.getClass.getName == t.getClass.getName)
    }

    def collect[T](f: PartialFunction[Tree, T]) = {
      val hits = new ListBuffer[T]
      object collectTreeTraverser extends Traverser {
        override def traverse(t: Tree) {
          if (f.isDefinedAt(t)) hits += f(t)
          super.traverse(t)
        }
      }
      collectTreeTraverser.traverse(t)
      hits.toList
    }

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
          case t: DefDef if t.name.toString == "<init>" =>
            t.pos withStart t.pos.point withEnd (t.pos.point + "this".length)
          case ValDef(_, _, _, Match(_, CaseDef(unapply: UnApply , _, _) :: Nil)) if hasSingleBindWithTransparentPosition(unapply.args) =>
            // modify the position to remove the transparency..
            val b = findAllBinds(unapply.args).head
            b.pos withEnd b.namePosition.end
          case ValDef(_, _, _, Match(_, CaseDef(apply: Apply, _, _) :: Nil)) if hasSingleBindWithTransparentPosition(apply.args) =>
            val b = findAllBinds(apply.args).head
            b.pos withEnd b.namePosition.end
          // this is what a `lazy val Pattern(lazy) = ..` is translated to from 2.10.1 onwards
          case DefDef(_, _, _, _, _, Block(List(Assign(_, Match(_, CaseDef(apply: Apply, _, _) :: Nil))), _)) if hasSingleBindWithTransparentPosition(apply.args) =>
            val b = findAllBinds(apply.args).head
            b.pos withEnd b.namePosition.end
          case DefDef(_, _, _, _, _, Block(List(Assign(_, Match(_, CaseDef(unapply: Apply, _, _) :: Nil))), _)) if hasSingleBindWithTransparentPosition(unapply.args) =>
            val b = findAllBinds(unapply.args).head
            b.pos withEnd b.namePosition.end
          case t: ValOrDefDef =>

            val name = t.symbol match {
              case NoSymbol => t.name.toString.trim
              case ts: TermSymbol if ts.isLazy && ts.isMutable => ts.lazyAccessor.nameString
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
              new RangePosition(t.pos.source, t.pos.point, t.pos.point, t.pos.point + name.length)

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
            } else if (qualifier.pos.isRange && t.name.decode.endsWith(":")) {
              t.pos withEnd (t.pos.start + nameString.length)
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
            val src = stats.last.pos.source.content.slice(stats.last.pos.end, cond.pos.start).mkString
            val whileStart = stats.last.pos.end + src.indexOf("while")
            t.pos withStart whileStart withEnd (whileStart + "while".length)

          case t: SelectFromTypeTree =>
            t.pos withStart t.pos.point

          case t: DefTree => t.pos withStart t.pos.point withEnd (t.pos.point + nameString.length)

          case t: This =>
            t.pos withEnd (t.pos.start + t.qual.length)

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
            pos2.withPoint(pos2.start).makeTransparent
          else
            pos2.withPoint(pos2.start)
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
        // e.g. eta-expanded ValDefs and Idents
        else if (t.symbol.isSynthetic && t.pos.isTransparent)
          ""
        else if (name.toString.matches(".*\\$\\d+$"))
          "_"
        else if (t.symbol != NoSymbol) {
          t.symbol.nameString
        } else
          name.decode.toString.trim
      }

      t match {
        case t: DefDef if t.name.toString == "<init>" =>
          "this"
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
        case _ => sys.error("Tree "+ getSimpleClassName(t) +" does not have a name.")
      }
    }
  }

  implicit def additionalTreeMethodsForPositions(t: Tree) = new TreeMethodsForPositions(t)

  /**
   * Trees that reach the end of the file don't seem to have the correct end position,
   * except if there's a newline at the end.
   */
  def endPositionAtEndOfSourceFile(pos: Position, otherWise: Option[Int] = None) = {
    val lastCharInFile = pos.source.content(pos.end)
    if(pos.source.length -1 == pos.end
        && lastCharInFile != '\n'
        && lastCharInFile != '}'
        && lastCharInFile != ')'
        && lastCharInFile != ']')
       pos.end + 1
     else
       otherWise getOrElse pos.end
  }

  /**
   * Finds a tree by its position, can be used to find
   * the original tree from a transformed tree.
   *
   * If multiple trees are candidates, then take the last one,
   * because it is likely more specific.
   */
  def findOriginalTree: Tree => Option[Tree] = Memoized { tree =>

    val candidates = findAllTreesWithTheSamePosition(tree)

    candidates find {
      /*
       * When searching for the original of a NameTree, we
       * can additionally compare the names of the trees.
       * */
      case c: NameTree if tree.isInstanceOf[NameTree] =>
        c.nameString == tree.nameString
      case c =>

        /*
         * Some trees have the same type and position as their child
         * (e.g. in a for comprehension, the withFilter Select has the
         * same position as the underlying Select), in that case, we
         * also compare the structure of the trees.
         *
         * See RenameTest#renameMethodForComprehensionBody
         * */
        (c eq tree) || (c equalsStructure tree)
    } orElse (candidates.filter(_ samePosAndType tree).lastOption)
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

  /**
   * Returns the compilation unit root for that position.
   *
   * Note: there's also index.roots to get all compilation
   * units that are part of this refactoring.
   */
  private def cuRoot(p: Position): Option[Tree] = {
    if (p == NoPosition)
      None
    else {
      compilationUnitOfFile(p.source.file) map (_.body)
    }
  }

  implicit class DefDefMethods(defdef: DefDef) {

    def contextBounds: List[Tree] = {
      defdef.vparamss.lastOption.toList.flatten collect {
        case ValDef(mods, name, tpt: TypeTree, _)
          if mods.hasFlag(Flags.IMPLICIT)
          && name.toString.startsWith(nme.EVIDENCE_PARAM_PREFIX)
          && tpt.original.isInstanceOf[AppliedTypeTree] =>
            tpt.original.asInstanceOf[AppliedTypeTree].tpt
      }
    }

    def tparamsWithContextBounds: List[Tree] = {
      (defdef.tparams ++ contextBounds) sortBy (_.pos.startOrPoint)
    }

    def explicitVParamss: List[List[Tree]] = {
      if(contextBounds.isEmpty)
        defdef.vparamss
      else
        defdef.vparamss.init
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
    def primaryConstructor = t.body collect {
      case t: DefDef if t.symbol.isPrimaryConstructor || (t.symbol == NoSymbol && t.name.toString == nme.CONSTRUCTOR.toString) => t
    }

    /**
     * Returns all early definitions from the template body.
     */
    def earlyDefs = t.body.collect {
      case t @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if t.symbol.isConstructor => stats filter treeInfo.isEarlyDef
      case t @ DefDef(_, _, _, _, _, rhs) if t.symbol.isConstructor && treeInfo.isEarlyDef(rhs) => rhs :: Nil
    }.flatten

    /**
     * Returns the trees that are passed to a super constructor call.
     */
    def superConstructorParameters = t.body.collect {
      case d @ DefDef(_, _, _, _, _, BlockExtractor(stats)) if d.symbol.isConstructor || d.name.toString == nme.CONSTRUCTOR.toString =>
        stats.collect {
          // we need to exclude calls to this class' constructors, this seems to catch them:
          case a @ Apply(_, args) if a.tpe != t.tpe || (a.tpe == null && t.tpe == null) =>
            args
        }.flatten
    }.flatten

    /**
     * Returns all constructor parameters that are accessible from outside the class itself.
     * The mutability of the parameter is contained in the second element of the tuple.
     */
    def nonPrivateClassParameters = {
      val primaryConstructor = t.primaryConstructor.headOption
      primaryConstructor match {
        case None => Nil
        case Some(pc) => {
          val classParams = pc.vparamss.flatten
          val paramAccessors = (t.body collect {
            case defdef @ DefDef(mods, _, _, _, _, _) if mods.hasFlag(Flags.ACCESSOR) => defdef
          })

          val mutableParamNames = (t.body collect {
            case valdef @ ValDef(mods, _, _, _) if mods.isParamAccessor && mods.isMutable => valdef.nameString
          })
          val paramMutability = Map(paramAccessors.map(d => (d.nameString, mutableParamNames contains d.nameString)): _*)
          val paramAccessorNames = paramAccessors.map(_.nameString)
          val nonPrivateClassParams = classParams.collect { case p: ValDef if paramAccessorNames.contains(p.nameString) => (p, paramMutability(p.nameString)) }
          nonPrivateClassParams
        }
      }
    }

    /**
     * Returns existing equality methods.
     * Note that this is a rough by-name check.
     */
    def existingEqualityMethods: List[ValOrDefDef] = {
      t.body collect {
        case d: ValOrDefDef if List(nme.equals_, nme.hashCode_, nme.canEqual_) contains d.name => d
      }
    }

    /**
     * Returns whether the template has an implementation of an equals, canEquals or hashCode method.
     */
    def hasEqualityMethod: Boolean = existingEqualityMethods match {
      case Nil => false
      case _ => true
    }

    /**
     * Returns true if this template belongs to an anonymous class.
     */
    def isTemplateForAnonymousClass = t.primaryConstructor exists { t =>
      val sym = t.symbol
      sym != NoSymbol && sym.owner.isAnonymousClass
    }
  }

  implicit def additionalTemplateMethods(t: Template) = new TemplateMethods(t)

  /**
   * Provides a finer-grained extractor for Template that distinguishes
   * between class constructor parameters, early definitions, parents,
   * self type annotation and the real body.
   */
  object TemplateExtractor {

    def unapply(t: Tree): Option[(List[List[ValDef]], List[Tree], List[Tree], Tree, List[Tree])] = t match {
      case tpl: Template =>

        val pimpedTpl = additionalTemplateMethods(tpl)

        val classParams = {

          val primaryConstructorArgs = pimpedTpl.primaryConstructor flatMap (_.vparamss) map (_.size)

          def groupPrimaryConstructorArgs(groups: List[Int], fields: List[ValDef]): List[List[ValDef]] = groups match {
            case Nil => Nil
            case n :: ns =>
              val (current, rest) = fields.splitAt(n)
              current :: groupPrimaryConstructorArgs(ns, rest)
          }

          val constructorParameters = pimpedTpl.constructorParameters
          if(primaryConstructorArgs.sum != constructorParameters.size) {
            List(constructorParameters)
          } else {
            groupPrimaryConstructorArgs(primaryConstructorArgs, constructorParameters)
          }
        }

        val body = {
          val bodyWithoutPrimaryConstructorAndArgs = tpl.body filterNot ((pimpedTpl.primaryConstructor ::: pimpedTpl.constructorParameters).contains)
          val removeGeneratedTrees = bodyWithoutPrimaryConstructorAndArgs filter keepTree
          removeCompilerTreesForMultipleAssignment(removeGeneratedTrees)
        }

        val parents = (pimpedTpl.superConstructorParameters match {
          case Nil =>
            tpl.parents
          case params =>
            SuperConstructorCall(tpl.parents.head, params) :: tpl.parents.tail
        }) filterNot (isEmptyTree) filter {
          // objects are always serializable, but the Serializable parent's position is NoPosition
          case t: TypeTree if t.pos == NoPosition && t.nameString == "Serializable" => false
          case t => t.pos.isRange || t.pos == NoPosition
        }

        val self = if(isEmptyTree(tpl.self)) EmptyTree else {

          if(tpl.pos.isRange) {

            val selfNameTree = if(tpl.self.name.toString == "_") {
              NameTree("this") setPos (tpl.self.pos withEnd (tpl.self.pos.start + "this".length))
            } else {
              NameTree(tpl.self.name) setPos {
                val p = tpl.self.pos
                p withEnd (if(p.start == p.point) p.end else p.point)
              }
            }

            SelfTypeTree(selfNameTree, tpl.self.tpt) setPos tpl.self.pos
          } else {
            tpl.self
          }
        }

        Some(Tuple5(classParams, pimpedTpl.earlyDefs, parents, self, body))

      case _ =>
        None
    }
  }

  /**
   * Returns all children that have a representation in the source code.
   * This includes Name and Modifier trees and excludes everything that
   * has no Position or is an EmptyTree.
   */
  def children: Tree => List[Tree] = Memoized { t =>
    val ch = t match {

      case PackageDef(pid, stats) =>
        pid :: stats

      case t @ ClassDef(ModifierTree(mods), name, tparams, impl) =>
        mods ::: (NameTree(name) setPos t.namePosition) :: tparams ::: impl :: Nil

      case t @ ModuleDef(ModifierTree(mods), name, impl) =>
        mods ::: (NameTree(name) setPos t.namePosition) :: impl :: Nil

      case TemplateExtractor(params, earlyBody, parents, self, body) =>
        params.flatten ::: earlyBody ::: parents ::: self :: body

      case t @ ValDef(ModifierTree(mods), name, tpt, rhs) if !t.pos.isTransparent =>
        mods ::: (NameTree(name) setPos t.namePosition) :: tpt :: rhs :: Nil

      case t @ DefDef(ModifierTree(mods), name, _, _, tpt, rhs) =>
        mods ::: (NameTree(name) setPos t.namePosition) :: t.tparamsWithContextBounds ::: t.explicitVParamss.flatten ::: tpt :: rhs :: Nil

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
        // FIXME: for-comprehensions result in incorrect NameTrees
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

      case SelfTypeTree(name, tpt) =>
        name :: tpt :: Nil

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
      case LabelDef(name, params, If(cond, ifTrue, _)) =>
        (NameTree(name) setPos t.namePosition) :: params ::: cond :: ifTrue :: Nil

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

      case NamedArgument(nme, rhs) =>
        nme :: rhs :: Nil

      case MultipleAssignment(extractor, values, rhs) =>
        extractor :: values ::: rhs :: Nil

      case DocDef(_, definition) =>
        definition :: Nil

      case _ => Nil
    }

    ch filter keepTree
  }

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
       case (v @ ValDef(_, _, _, Match(Typed(rhs, _), (c @ CaseDef(_: Apply, EmptyTree, body)) :: Nil))) :: xs
          if v.symbol.isSynthetic && (c.pos.isTransparent || c.pos == NoPosition) =>
        mkMultipleAssignment(EmptyTree, body.tpe, v.pos, rhs, xs)

      case (v @ ValDef(_, _, _, Match(Typed(rhs, _), (c @ CaseDef(extractor: UnApply, EmptyTree, body)) :: Nil))) :: xs
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

  def keepTree(t: Tree) = !isEmptyTree(t) && (t.pos.isRange || t.pos == NoPosition)

  /**
   * Represent a Name as a tree, including its position.
   */
  case class NameTree(name: global.Name) extends global.Tree {
    if (name == nme.NO_NAME) sys.error("Name cannot be <none>, NoSymbol used?")
    def nameString = {
      if(pos.isRange && pos.source.content(pos.start) == '`' && !name.toString.startsWith("`")) {
        "`"+ name.decode.trim +"`"
      } else {
        name.decode.trim
      }
    }
    override def toString = "NameTree("+ nameString +")"
  }
  object NameTree {
    def apply(name: String) = new NameTree(newTermName(name))
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
      case _            => "<unknown flag, please report a bug>"
    }
  }

  /**
   * Extract the modifiers with their position from a Modifiers
   * object.
   */
  object ModifierTree {
    def unapply(m: global.Modifiers) = {
      Some(m.positions.toList map {
        // hack to get rid of override modifiers
        // couldn't figure out how to remove a flag from the positions map (michael)
        case (Flags.OVERRIDE, _) if (m.flags & Flags.OVERRIDE) == 0 => ModifierTree(0)
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
    if(clazz.pos.isRange) {
      val lastPos = args.lastOption map (_.pos) filter (_.isRange) map { pos =>
        /*
         * We want to include all the layout including the ) in the position.
         * */
        val layout = Layout(pos.source, pos.end, pos.source.length)
        val countToParens = layout.withoutComments.takeWhile(_ != ')').size
        pos.withEnd(pos.end + countToParens + 1)
      } getOrElse clazz.pos
      setPos(clazz.pos withEnd lastPos.end)
    }
  }

  /**
   * Representation of self type annotations:
   *   self: A with B =>
   *   ^^^^^^^^^^^^^^
   */
  case class SelfTypeTree(name: pimpedTrees.NameTree, tpt: Tree) extends global.Tree

  case class NamedArgument(nameTree: pimpedTrees.NameTree, rhs: Tree) extends global.RefTree {
    def qualifier = EmptyTree
    val name = nameTree.name
  }

  object ApplyExtractor {

    def couldHaveDefaultArguments(t: Tree) = t match {
      case Apply(qualifier, args) =>

        def isSetter = PartialFunction.cond(qualifier) {
          case t: Select => t.name.endsWith(nme.EQL)
        }

        def isVarArgsCall = {
          // TODO is there a better check?
          qualifier.tpe.params.size != args.size
        }

        t.pos.isRange &&
        qualifier.pos.isRange &&
        qualifier.tpe != null &&
        !args.isEmpty &&
        !isSetter &&
        !isVarArgsCall &&
        args.exists(_.pos.isRange)
      case _ => false
    }

    def unapply(t: Apply): Option[(Tree, List[Tree])] = extract(t)

    val extract: Apply => Option[(Tree, List[Tree])] = Memoized {

      case t @ Apply(qualifier, args) if couldHaveDefaultArguments(t) =>

        val declaredParameterSyms = qualifier.tpe.params

        val argsWithPreviousTree = (qualifier :: args).sliding(2, 1).toList

        val transformedArgs = argsWithPreviousTree zip declaredParameterSyms map {
          case (List(leading, argument), sym) if argument.pos.isRange =>

            val src = Layout(leading.pos.source, leading.pos.end, argument.pos.start).withoutComments
            val isNamedArgument = {
              // The second clause tests for the case where there's an update method
              // that's called without named arguments, like `obj() = 1'.
              src.contains("=") && !src.matches(".*\\).*=.*")
            }

            if(isNamedArgument) {
              val start = leading.pos.end + src.indexOf(sym.decodedName)
              val end = start + sym.decodedName.length
              val namePos = argument.pos withStart start withPoint start withEnd end

              new NamedArgument(NameTree(sym.name) setPos namePos, argument) {
                setSymbol(sym)
                setPos(argument.pos withStart start withPoint argument.pos.start)
              }
            } else {
              argument
            }

          case (List(leading, argument), sym) => argument
        }

        Some(Pair(t.fun, transformedArgs))

      case t =>
        Some(Pair(t.fun, t.args))
    }
  }
  /**
   * Unify the children of a Block tree and sort them
   * in the same order they appear in the source code.
   *
   * Also reshapes some trees: multiple assignments are
   * removed and named argument trees are created.
   */
  object BlockExtractor {

    def unapply(t: Block) = {

      def fixNamedArgumentCall(block: Block): Tree = block match {
        case Block(stats, apply @ Apply(fun: Select, emptyArgs)) if apply.pos.isRange && emptyArgs.size == stats.size && emptyArgs.forall(i => isEmptyTree(i) || !i.pos.isRange) =>

          val allValDefs = stats forall {
            case t: ValDef => t.pos.isRange && t.pos.start > apply.pos.start
            case _ => return block
          }

          val argumentSymbols = fun.tpe match {
            case tpe: MethodType => tpe.params
            case _ => return block
          }

          // The arguments of apply all have an offset position, so they
          // were removed during the transformations. Therefore we have
          // to look up the original apply method
          val argumentsFromOriginalTree = compilationUnitOfFile(apply.pos.source.file) map (_.body) flatMap { root =>
            val treeWithSamePos = root.find(_ samePos apply)
            treeWithSamePos collect {
              case Block(_, Apply(_, args)) => args
              case Apply(_, args) => args
            }
          } getOrElse (return block)

          val syntheticNameToSymbol = argumentsFromOriginalTree.map {
            case a: Ident => a.name
            case _ => return block
          }.zip(argumentSymbols).toMap

          val startOffset = apply.pos.point
          // FIXME strip comments!
          val argumentsSource = apply.pos.source.content.slice(startOffset, apply.pos.end).mkString

          val newValDefs = stats collect {
            case t: ValDef if t.pos != NoPosition=>
              val sym = syntheticNameToSymbol(t.name)

              val newVal = NamedArgument(NameTree(sym.name), t.rhs) setSymbol sym

              // FIXME we can do a better search..
              val nameStart = argumentsSource.indexOf(newVal.name.toString)

              if(nameStart >= 0) {
                val nameLength = newVal.name.length
                val namePos = (t.pos withStart (nameStart + startOffset) withPoint nameStart + startOffset + nameLength)
                newVal.nameTree setPos namePos
                newVal setPos namePos.withEnd(t.pos.end)
                newVal
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

  case class MultipleAssignment(extractor: Tree, names: List[ValDef], rhs: Tree) extends global.Tree

  /**
   * Converts a tree containing Idents and Selects to a `.` separated string.
   */
  def asSelectorString(t: Tree) = {
    t.filter(_ => true).map {
      case Ident(name) => name.toString
      case Select(_, name) => name.toString
      case _ => ""
    }.reverse.mkString(".")
  }

  /**
   * @return Returns the (symbol) ancestors of the tree excluding the ROOT
   * in descending order. Also filters the symbols for package objects!
   */
  def ancestorSymbols(t: Tree): List[Symbol] = {
    t.symbol.ownerChain.takeWhile(_.nameString != nme.ROOT.toString).filterNot(_.isPackageObjectClass).reverse
  }

  /**
   * @return Returns the most specific package declaration in the compilation
   * unit. For example, given the following declaration:
   *
   *   package a
   *   package b
   *
   *   class C
   *
   * it returns `b`. If there are further nested packages, they are ignored:
   *
   *   package a
   *   class C
   *   package b
   *
   * returns `a`.
   */
  def topPackageDef(t: PackageDef): PackageDef = {
    t find {
      case PackageDef(_, NoPackageDef(_) :: _) => true
      case _ => false
    } collect {
      case pkg: PackageDef => pkg
    } getOrElse {
      t
    }
  }

  def isClassTag(c: Constant): Boolean = c.tag == ClazzTag

  /**
   * Returns whether the tree is considered empty.
   *
   * Prior to Scala 2.10.1 it was sufficient to check Tree#isEmpty,
   * but now we also need to check if the tree is equal to emptyValDef.
   */
  def isEmptyTree(t: Tree): Boolean = t.eq(emptyValDef) || t.isEmpty

  class NotInstanceOf[T](implicit m: Manifest[T]) {
    def unapply(t: Tree): Option[Tree] = {
      if(m.runtimeClass.isInstance(t)) {
        None
      } else
        Some(t)
    }
  }

  object NoBlock extends NotInstanceOf[Block]
  object NoPackageDef extends NotInstanceOf[PackageDef]
  object NoFunction extends NotInstanceOf[Function]
  object NoImportSelectorTree extends NotInstanceOf[ImportSelectorTree]


  /**
   *  The PlainText "tree" provides a hook into the source code generation.
   *  When a PlainText tree occurs during source code generation, its `print`
   *  method is called with the current AbstractPrinter#PrintingContext. The
   *  result is inserted into the generated source code.
   *
   *  For some use cases (blank line, raw and indented string) implementations
   *  already exist in the `PlainText` object.
   *
   *  Note that PlainText trees should never be allowed to escape the Scala
   *  refactoring library, so be careful when using compiler utilities to
   *  transform trees.
   */
  abstract class PlainText extends global.Tree {
    def print(ctx: AbstractPrinter#PrintingContext): Fragment
  }

  object PlainText {

    /**
     * Inserts a blank line into the generated source code.
     */
    case object BlankLine extends PlainText {
      def print(ctx: AbstractPrinter#PrintingContext) = {
        Fragment(ctx.newline + ctx.newline + ctx.ind.current)
      }
    }

    /**
     * Inserts `text` verbatim into the source code.
     *
     * Note: to automatically indent the string, use `Indented`.
     */
    case class Raw(text: String) extends PlainText {
      def print(ctx: AbstractPrinter#PrintingContext) = {
        Fragment(text)
      }
    }

    /**
     * Indents `text` to the current indentation level and
     * inserts it into the generated code.
     */
    case class Indented(text: String) extends PlainText {
      def print(ctx: AbstractPrinter#PrintingContext) = {
        Fragment(text.replaceAll(ctx.newline, ctx.newline + ctx.ind.current))
      }
    }
  }

  /**
   * A SourceLayoutTree can be used to insert arbitrary text into the code,
   * for example, blank lines.
   *
   */
  @deprecated("Use PlainText objects and its components", "0.5.0")
  case class SourceLayoutTree(kind: SourceLayouts.Kinds) extends global.Tree

  @deprecated("Use PlainText objects and its components", "0.5.0")
  object SourceLayouts {
    sealed trait Kinds
    @deprecated("Use PlainText.Newline instead", "0.5.0")
    object Newline extends Kinds
  }
}
