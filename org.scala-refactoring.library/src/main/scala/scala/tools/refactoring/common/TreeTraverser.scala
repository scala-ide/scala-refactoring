/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package common

import tools.nsc.io.AbstractFile
import collection.mutable.ListBuffer
import tools.nsc.Global
import tools.nsc.util.RangePosition

trait TreeTraverser {

  this: CompilerAccess with common.PimpedTrees =>

  import global._

  /**
   *  A traverser that also traverses a TypeTree's original type.
   */
  trait Traverser extends global.Traverser {
    override def traverse(t: Tree) = t match {
      case t: TypeTree if t.original != null =>
        traverse(t.original)
      case t =>
        super.traverse(t)
     }
  }

  /**
   *  A traverser that creates fake trees for various
   *  type trees so they can be treated as if they were
   *  regular trees.
   */
  trait TraverserWithFakedTrees extends global.Traverser {

    def fakeSelectTreeFromType(tpe: Type, sym: Symbol, pos: Position) = {
      // we fake our own Select(Ident(..), ..) tree from the type
      // so we can handle them just like any other select call

      val stringRep = tpe.trimPrefix(tpe.toString)

      val select = stringRep.split("\\.").toList match {
        case x :: xs =>
          xs.foldLeft(Ident(x): Tree) {
            case (inner, outer) => Select(inner, outer)
          }
        case Nil => EmptyTree
      }
      select.setType(tpe).setSymbol(sym).setPos(pos)
    }

    def fakeSelectTree(tpe: Type, sym: Symbol, tree: Tree): Tree = {

      val flattenedExistingTrees = tree.filter(_ => true) map {
        case t: Ident =>  (t.name.toString, t.pos)
        case t: Select => (t.name.toString, t.pos)
        case _ => return tree
      }

      val treesFromType = {
        val tpeWithoutPrefix = tpe.trimPrefix(tpe.toString).split("\\.") match {
          case tpes if tpe.isInstanceOf[SingletonType] => tpes.init // drop the `.type`
          case tpes => tpes
        }
        tpeWithoutPrefix.toList.reverse zip Stream.continually(NoPosition)
      }

      val fullPathOfAllTrees = (flattenedExistingTrees ++ treesFromType.drop(flattenedExistingTrees.size)).reverse

      def symbolAncestors(s: Symbol): Stream[Symbol] = if(s == NoSymbol) Stream.continually(NoSymbol) else Stream.cons(s, symbolAncestors(s.owner))

      val select = fullPathOfAllTrees zip symbolAncestors(sym).take(fullPathOfAllTrees.length).reverse.toList match {
        case ((x: String, pos: Position), sym: Symbol) :: xs =>
          val i = Ident(x).setPos(pos).setSymbol(sym)
          xs.foldLeft(i: Tree) {
            case (inner: Ident, ((outer, pos), sym)) if outer == "this" =>
              new This(inner.name.toTypeName).setPos(pos).setSymbol(sym)
            case (inner: Ident, ((outer, pos), sym)) if outer == "package" =>
              inner
            case (inner, ((outer, pos), sym)) =>
              Select(inner, outer).setPos(pos).setSymbol(sym)
          }
        case Nil => EmptyTree
      }

      select.setType(tpe)

      select
    }

    def handleAnnotations(as: List[AnnotationInfo]) {
      as map (_.original) foreach traverse
    }

    def handleAppliedTypeTree(tree: AppliedTypeTree, tpe: TypeRef): Unit = (tree, tpe) match {
      case (tree @ AppliedTypeTree(tpt, treeArgs), tpe @ TypeRef(_, sym, tpeArgs)) =>

        val t = fakeSelectTree(sym.tpe, sym, tpt)
        traverse(t)

        tpeArgs zip treeArgs foreach {
          case (tpe: TypeRef, tree: AppliedTypeTree) =>
            handleAppliedTypeTree(tree, tpe)
          case (TypeRef(_, sym, _), tree) =>
            fakeSelectTree(sym.tpe, sym, tree) foreach traverse
          case (tpe, tree) =>
            // TODO ?
        }
    }

    override def traverse(t: Tree) = {

      Option(t.symbol) filter (_.pos.isRange) foreach (s => handleAnnotations(s.annotations))

      t match {
        // The standard traverser does not traverse a TypeTree's original:
        case t: TypeTree if t.original != null =>

          def handleCompoundTypeTree(parents: List[Tree], parentTypes: List[Type]) = {
            parents zip parentTypes foreach {

              case (i @ Ident(name), tpe @ TypeRef(_, sym, _)) if i.tpe == null =>
                fakeSelectTree(tpe, sym, i) foreach traverse

              case (tree, _) => traverse(tree)
            }
          }

          (t.tpe, t.original) match {
            // in a self type annotation, the first tree is the trait itself, so we skip that one

            // self type annotation with a single type
            case (RefinedType(_ :: tpe :: Nil, _), tree: Ident) =>
              handleCompoundTypeTree(List(tree), List(tpe))

            // self type annotation with a compound type
            case (RefinedType(_ :: RefinedType(parentTypes, _) :: Nil, _), CompoundTypeTree(Template(parents, self, body))) =>
              handleCompoundTypeTree(parents, parentTypes)

            // handle regular compound type trees
            case (RefinedType(parentTypes, _), CompoundTypeTree(Template(parents, self, body))) =>
              handleCompoundTypeTree(parents, parentTypes)

            case (tpe, SingletonTypeTree(tree)) if tpe != null =>
              tpe.widen match {
                case tpe @ TypeRef(_, sym, _) =>
                  fakeSelectTree(tpe, sym, tree) foreach traverse
                case _ =>
                  traverse(t.original)
              }

            case (tpe: TypeRef, tpt: AppliedTypeTree) if tpe != null =>
              handleAppliedTypeTree(tpt, tpe)
              traverse(tpt)

            case (AnnotatedType(annotations, underlying, selfsym), _) =>
              handleAnnotations(annotations)
              traverse(t.original)

            case (ExistentialType(quantified, TypeRef(_, sym, _)), ExistentialTypeTree(AppliedTypeTree(tpt, _), _)) =>
              fakeSelectTree(sym.tpe, sym, tpt) foreach traverse

            case (tpe: TypeRef, ident: Ident)
                if tpe.sym.pos == NoPosition || (tpe.sym.pos != NoPosition && tpe.sym.pos.source != t.pos.source) =>
              fakeSelectTreeFromType(tpe, tpe.sym, ident.pos) foreach traverse

            case _ =>
              traverse(t.original)
          }
        case t =>
          super.traverse(t)
      }
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends global.FilterTreeTraverser(p) with Traverser

  def filterTree(t: Tree, traverser: global.FilterTreeTraverser) = {
    traverser.traverse(t)
    traverser.hits.toList
  }

  class TreeWithSymbolTraverser(f: (Symbol, Tree) => Unit) extends Traverser {

    override def traverse(t: Tree) = {

      t match {

        /* For comprehensions are tricky, especially when combined with a filter:
         * The following code:
         *
         *   for (foo <- List("santa", "claus") if foo.startsWith("s")) yield foo
         *
         * is converted to
         *
         *   List("santa", "claus").withFilter(foo => foo.startsWith("s")).map(foo => foo)
         *
         * so we have several foo ValDefs/RefTrees that need to be related with
         * each other.
         */
        case Apply(generator, (Function(arg :: _, body)) :: Nil)
          if arg.pos.startOrPoint < generator.pos.startOrPoint &&
             between(arg, generator).contains("<-") =>

          val referencesToVal = {
            val fromBody = body collect {
              case t: RefTree if t.name == arg.name => t
            }
            val fromGenerator = generator.collect {
              case Apply(Select(_, nme.withFilter), (Function((v: ValDef) :: _, body)) :: Nil)
                if !v.pos.isRange && v.name == arg.name =>
                  body collect {
                    case t: RefTree if t.symbol == v.symbol => t
                  }
            }.flatten

            fromBody ++ fromGenerator
          }

          val valDefs = arg :: {
            generator collect {
              case Apply(Select(_, nme.withFilter), (Function((v: ValDef) :: _, _)) :: Nil)
                if !v.pos.isRange && v.name == arg.name =>
                  v
            }
          }

          referencesToVal foreach { ref =>
            valDefs foreach { valDef =>
              f(valDef.symbol, ref)
              f(ref.symbol, valDef)
            }
          }

        case t: TypeTree if t.original != null =>

          (t.original, t.tpe) match {
            case (att @ AppliedTypeTree(_, args1), tref @ TypeRef(_, _, args2)) =>
              args1 zip args2 foreach {
                case (i: RefTree, tpe: TypeRef) =>
                  f(tpe.sym, i)
                case _ => ()
              }
            case (ExistentialTypeTree(AppliedTypeTree(tpt, _), _), ExistentialType(_, underlying: TypeRef)) =>
              f(underlying.sym, tpt)
            case (t, TypeRef(_, sym, _)) =>
              f(sym, t)
            case _ => ()
          }

        case t: ClassDef if t.symbol != NoSymbol =>

          /* Class parameters passed to super constructors:
           *
           *  class Sub(a: String) extends Base(a)
           *
           * are actually represented like this:
           *
           *  class Sub extends Base {
           *    <paramaccessor> private[this] val a: String = _
           *    def this(a: String) = {
           *      Sub.super.this(a)
           *    }
           *  }
           *
           * So we need to manually create the link between the
           * paramaccessor and the argument to super:
           * */

          val superConstrArgs = t.impl.superConstructorParameters

          superConstrArgs foreach { superArg =>

            val constrParamAccessors = t.symbol.constrParamAccessors

            superArg filter {
              case i: Ident => true
              case _ => false
            } foreach { superArg =>

              constrParamAccessors.find { param =>
                // trim because the name might have a trailing ' ' when it's a val or var
                val n1 = param.name.toString.trim
                val n2 = superArg.symbol.name.toString
                n1 == n2
              } foreach { sym =>
                f(sym, superArg)
              }

              val constructorParameters = t.impl.constructorParameters

              constructorParameters.find(_.name == superArg.symbol.name) foreach { t =>
                // we also add a reference from the super argument's symbol to the
                // constructor parameter accessor
                f(superArg.symbol, t)
              }

            }
          }

          f(t.symbol, t)

//        case BlockExtractor(stats) =>
//          stats foreach traverse
//
//        case ApplyExtractor(fun, args) =>
//          traverse(fun)
//          args foreach traverse

        case t: DefTree if t.symbol != NoSymbol =>
          f(t.symbol, t)
        case t: RefTree =>
          f(t.symbol, t)
        case t: TypeTree if t.original == null =>

          def handleType(typ: Type): Unit = typ match {
            case RefinedType(parents, _) =>
              parents foreach handleType
            case TypeRef(_, sym, _) =>
              f(sym, t)
            case _ => ()
          }

          handleType(t.tpe)

        case t @ Import(expr, selectors) if expr.tpe != null =>

          t.Selectors() foreach { selector =>
            findSymbolForImportSelector(expr, selector.name.name) foreach { sym =>
              f(sym, selector)
            }
          }

        /*
         * classOf[some.Type] is represented by a Literal
         * */
        case t @ Literal(Constant(value)) =>

          value match {
            case TypeRef(_, sym, _) =>
              f(sym, t)
            case _ => ()
          }

        case t: This if t.pos.isRange =>
          f(t.symbol, t)

        case _ => ()
      }

      t match {
        case _: NamedArgument | _: NameTree | _: MultipleAssignment =>
          ()
        case t =>
          super.traverse(t)
      }
    }

    private def between(t1: Tree, t2: Tree) = {
      if(t1.pos.isRange && t2.pos.isRange)
        t1.pos.source.content.slice(t1.pos.end, t2.pos.start).mkString
      else ""
    }
  }
}
