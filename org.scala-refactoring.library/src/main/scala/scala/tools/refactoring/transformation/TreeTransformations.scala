/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import language.implicitConversions

trait TreeTransformations extends Transformations with TreeFactory {

  this: common.PimpedTrees with common.CompilerAccess =>

  import global._

  def traverse(tree: Tree, f: Tree => Tree): Tree = {

    /**
     * Hooks into the Scala compiler's Transformer but applies only
     * one transformation and then returns that result.
     */
    object TransformOnce extends Transformer {

      /**
       * Transforms the children of the trees using `f` and creates
       * a new t with the transformed children
       */
      def once(tree: Tree) = tree match {

        case NamedArgument(name, rhs) =>
          transform(rhs) match {
            case `rhs` => tree
            case rhs =>
              NamedArgument(name, rhs)
          }

        case _: ImportSelectorTree | _: SourceLayoutTree | _: PlainText =>
          tree

        case MultipleAssignment(extractor, vals, rhs) =>
          (transform(extractor), transformTrees(vals), transform(rhs)) match {
            case (`extractor`, `vals`, `rhs`) => tree
            case (e, v, r) => MultipleAssignment(e, v.asInstanceOf[List[ValDef]], r)
          }

        case t: TypeTree if t.original != null =>
          val transformedTypeTree = super.transform(t).asInstanceOf[TypeTree]
          val transformedOriginal = f(t.original)

          // if only the original tree has been transformed, we have to create
          // a new TypeTree instance so the old and new ones are not `eq`.
          if(transformedTypeTree.eq(t) && !transformedOriginal.eq(t.original)) {
            new TypeTree().copyAttrs(t).setOriginal(transformedOriginal)
          } else {
            transformedTypeTree.setOriginal(transformedOriginal)
          }

        case t: UnApply =>
          // super does not transform t.fun
          treeCopy.UnApply(tree, transform(t.fun), transformTrees(t.args))

        case t if t.tpe != null && t.tpe.isError => t

        case t => super.transform(t)
      }

      override def transform(t: Tree) = f(t)
    }

    TransformOnce.once(tree)
  }

  def transform(f: PartialFunction[Tree, Tree]) = transformation(f)

  def filter(f: PartialFunction[Tree, Boolean]) = predicate(f)

  def replaceTree(from: Tree, to: Tree) = ↓(matchingChildren(predicate((t: Tree) => t samePosAndType from) &> constant(to)))

  implicit class TreeReplacesOtherTreeViaPosition[T <: Tree](t1: T) {
    def replaces[T2 >: T <: Tree](t2: T2): T = {
      t1 setPos t2.pos
    }
  }

  def abstractFileToTree(file: tools.nsc.io.AbstractFile): global.Tree = compilationUnitOfFile(file).get.body

  /**
   * Replace the first sequence of elements with another sequence.
   */
  implicit class AdditionalListMethods[T](l: List[T]) {
    def replaceSequence(what: List[T], replacement: List[T]): List[T] = {
      def inner(from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
        case (Nil, _) => Nil
        case (xs, Nil) => xs
        case (x :: xs, y :: ys) if x == y => replacement ::: inner(xs, ys, Nil)
        case (x :: xs, _) => x :: inner(xs, what, replacement)
      }
      inner(l, what, replacement)
    }
  }

  /**
   * Locates the imports in a PackageDef. If we have nested packages, it will only match in the innermost.
   */
  val locatePackageLevelImports = {

    def splitImports(p: PackageDef, stats: List[Tree]) = {
      val (imports, others) = stats partition (_.isInstanceOf[Import])
      (p, imports map (_.asInstanceOf[Import]), others)
    }

    transformation[Tree, (PackageDef, List[Import], List[Tree])] {
      case p @ PackageDef(_, stats @ (NoPackageDef(_) :: _)) =>
        splitImports(p, stats)
      case p @ PackageDef(_, stats) if stats.filter(_.isInstanceOf[PackageDef]).size > 1 =>
        splitImports(p, stats)
    }
  }

  def shallowDuplicate[T <: Tree](orig: T): T = {
    new Transformer {
      override val treeCopy = new StrictTreeCopier
      override def transform(tree: Tree) = {
        if (tree eq orig)
          super.transform(tree)
        else
          tree
      }
    } transform orig
  }.asInstanceOf[T]


  val setNoPosition = transform {
    case t: global.Tree => t.pos = global.NoPosition; t
  }

  def addImportTransformation(importsToAdd: Iterable[String]): Transformation[Tree, Tree] = {

    import global._

    def importTrees = {
      val SplitAtDot = "(.*)\\.(.*?)".r
      importsToAdd.map {
        case SplitAtDot(pkg, name) => mkImportFromStrings(pkg, name)
      }.toList
    }

    val addImportStatement = once(locatePackageLevelImports &> transformation[(PackageDef, List[Import], List[Tree]), Tree] {

      // For an empty PackageDef, with no exiting imports but with an Impl that has an annotation, we need to
      // modify positions so that the annotation, which is not in the AST, doesn't get assigned to the PackageDef
      // but to the Impl
      case (p @ PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), _), Nil, others @ (impl :: _)) if !impl.symbol.annotations.isEmpty =>

        // The `pid` is invisible anyway, but by erasing its position we make sure
        // it doesn't get any layout associated
        val ignoredPid = p.pid setPos NoPosition

        // The empty PackageDef starts at the position of its first child, so the annotation of the Impl
        // is outside its parent's range. The Source Generator can't handle this, so we let the PackageDef
        // start at position 0 so that the annotation gets associated to the child.
        val pos = p.pos withStart 0

        p copy (pid = ignoredPid, stats = (importTrees ::: others)) setPos pos

      case (p, imports, others) =>
        p copy (stats = (imports ::: importTrees ::: others)) replaces p
    })

    // first try it at the top level to avoid traversing the complete AST
    addImportStatement |> topdown(matchingChildren(addImportStatement))
  }
}
