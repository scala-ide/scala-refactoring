/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

trait TreePrintingTraversals {

  self: common.Tracing with common.PimpedTrees with Indentations with common.CompilerAccess with AbstractPrinter =>

  import global._

  trait TreePrinting {

    printer =>

    def dispatchToPrinter(t: Tree, ctx: PrintingContext): Fragment = context("Printing Tree "+ getSimpleClassName(t)) {

      trace("current indentation set to %s", ctx.ind.current)

      implicit val newCtx = ctx.copy(parent = t)

      val code = t match {
        case tree @ PackageDef(pid, stats) => printer.PackageDef(tree, pid, stats)
        case tree @ ClassDef(ModifierTree(mods), name, tparams, impl) => printer.ClassDef(tree, mods, name, tparams, impl)
        case tree @ ModuleDef(ModifierTree(mods), name, impl) => printer.ModuleDef(tree, mods, name, impl)
        case tree @ ValDef(ModifierTree(mods), name, tpt, rhs) => printer.ValDef(tree, mods, name, tpt, rhs)
        case tree @ DefDef(ModifierTree(mods), name, tparams, vparamss, tpt, rhs) => printer.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
        case tree @ TypeDef(ModifierTree(mods), name, tparams, rhs) => printer.TypeDef(tree, mods, name, tparams, rhs)
        case tree @ LabelDef(name, params, rhs) => printer.LabelDef(tree, name, params, rhs)
        case tree @ Import(expr, selectors) => printer.Import(tree, expr, tree.Selectors())
        case tree @ DocDef(comment, definition) => printer.DocDef(tree, comment, definition)
        case tree @ Template(parents, self, body) => printer.Template(tree, parents, self, body)
        case tree @ BlockExtractor(stats) => printer.Block(tree, stats)
        case tree @ CaseDef(pat, guard, body) => printer.CaseDef(tree, pat, guard, body)
        case tree @ Alternative(trees) => printer.Alternative(tree, trees)
        case tree @ Star(elem) => printer.Star(tree, elem)
        case tree @ Bind(name, body) => printer.Bind(tree, name, body)
        case tree @ UnApply(fun, args) => printer.UnApply(tree, fun, args)
        case tree @ ArrayValue(elemtpt, trees) => printer.ArrayValue(tree, elemtpt, trees)
        case tree @ Function(vparams, body) => printer.Function(tree, vparams, body)
        case tree @ Assign(lhs, rhs) => printer.Assign(tree, lhs, rhs)
        case tree @ AssignOrNamedArg(lhs, rhs) => printer.AssignOrNamedArg(tree, lhs, rhs)
        case tree @ If(cond, thenp, elsep) => printer.If(tree, cond, thenp, elsep)
        case tree @ Match(selector, cases) => printer.Match(tree, selector, cases)
        case tree @ Return(expr) => printer.Return(tree, expr)
        case tree @ Try(block, catches, finalizer) => printer.Try(tree, block, catches, finalizer)
        case tree @ Throw(expr) => printer.Throw(tree, expr)
        case tree @ New(tpt) => printer.New(tree, tpt)
        case tree @ Typed(expr, tpt) => printer.Typed(tree, expr, tpt)
        case tree @ TypeApply(fun, args) => printer.TypeApply(tree, fun, args)
        case tree @ Apply(fun, args) => printer.Apply(tree, fun, args)
        case tree @ ApplyDynamic(qual, args) => printer.ApplyDynamic(tree, qual, args)
        case tree @ Super(qual, mix) => printer.Super(tree, qual, mix)
        case tree @ This(qual) => printer.This(tree, qual)
        case tree @ Select(qualifier, selector) => printer.Select(tree, qualifier, selector)
        case tree @ Ident(name) => printer.Ident(tree, name)
        case tree @ Literal(value) => printer.Literal(tree, value)
        case tree @ TypeTree() => printer.TypeTree(tree)
        case tree @ Annotated(annot, arg) => printer.Annotated(tree, annot, arg)
        case tree @ SingletonTypeTree(ref) => printer.SingletonTypeTree(tree, ref)
        case tree @ SelectFromTypeTree(qualifier, selector) => printer.SelectFromTypeTree(tree, qualifier, selector)
        case tree @ CompoundTypeTree(templ) => printer.CompoundTypeTree(tree, templ)
        case tree @ AppliedTypeTree(tpt, args) => printer.AppliedTypeTree(tree, tpt, args)
        case tree @ TypeBoundsTree(lo, hi) => printer.TypeBoundsTree(tree, lo, hi)
        case tree @ ExistentialTypeTree(tpt, whereClauses) => printer.ExistentialTypeTree(tree, tpt, whereClauses)
        case tree @ SelectFromArray(qualifier, selector, erasure) => printer.SelectFromArray(tree, qualifier, selector, erasure)
        // own trees
        case tree @ SuperConstructorCall(clazz, args) => printer.SuperConstructorCall(tree, clazz, args)
        case tree : ModifierTree => printer.ModifierTree(tree, tree.flag)
        case tree @ MultipleAssignment(extractor, values, rhs) => printer.MultipleAssignment(tree, extractor, values, rhs)
        case tree @ ImportSelectorTree(name, rename) => printer.ImportSelectorTree(tree, name, rename)
        case tree @ SelfTypeTree(name, tpt) => printer.SelfTypeTree(tree, name, tpt)
        case tree: SourceLayoutTree => printer.SourceLayoutTree(tree)
        case tree: NameTree => printer.NameTree(tree)
        case tree @ NamedArgument(name, rhs) => printer.NamedArgument(tree, name, rhs)
        // PlainText is a hook that allows the user to inject custom text into the output
        case tree: PlainText => tree.print(ctx)
      }

      trace("results in %s", code.asText)

      Fragment(code.asText)
    }

    def default(t: Tree)(implicit ctx: PrintingContext): Fragment = sys.error("Not implemented! "+ getSimpleClassName(t))

    def ClassDef(tree: ClassDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], impl: Template)(implicit ctx: PrintingContext): Fragment = default(tree)
    def PackageDef(tree: PackageDef, pid: RefTree, stats: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def ModuleDef(tree: ModuleDef, mods: List[ModifierTree], name: Name, impl: Template)(implicit ctx: PrintingContext): Fragment = default(tree)
    def ValDef(tree: ValDef, mods: List[ModifierTree], name: Name, tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def DefDef(tree: DefDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def TypeDef(tree: TypeDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def LabelDef(tree: LabelDef, name: Name, params: List[Tree], rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Import(tree: Import, expr: Tree, selectors: List[ImportSelectorTree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def DocDef(tree: DocDef, comment: DocComment, definition: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Template(tree: Template, parents: List[Tree], self: Tree, body: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Block(tree: Block, stats: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def CaseDef(tree: CaseDef, pat: Tree, guard: Tree, body: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Alternative(tree: Alternative, trees: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Star(tree: Star, elem: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Bind(tree: Bind, name: Name, body: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def UnApply(tree: UnApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def ArrayValue(tree: ArrayValue, elemtpt: Tree, trees: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Function(tree: Function, vparams: List[ValDef], body: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Assign(tree: Assign, lhs: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def AssignOrNamedArg(tree: AssignOrNamedArg, lhs: Tree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def If(tree: If, cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Match(tree: Match, selector: Tree, cases: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Return(tree: Return, expr: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Try(tree: Try, block: Tree, catches: List[Tree], finalizer: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Throw(tree: Throw, expr: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def New(tree: New, tpt: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Typed(tree: Typed, expr: Tree, tpt: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def TypeApply(tree: TypeApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Apply(tree: Apply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def ApplyDynamic(tree: ApplyDynamic, qual: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def Super(tree: Super, qual: Tree, mix: Name)(implicit ctx: PrintingContext): Fragment = default(tree)
    def This(tree: This, qual: Name)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Select(tree: Select, qualifier: Tree, selector: Name)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Ident(tree: Ident, name: Name)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Literal(tree: Literal, value: Constant)(implicit ctx: PrintingContext): Fragment = default(tree)
    def TypeTree(tree: TypeTree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def Annotated(tree: Annotated, annot: Tree, arg: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def SingletonTypeTree(tree: SingletonTypeTree, ref: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def SelectFromTypeTree(tree: SelectFromTypeTree, qualifier: Tree, selector: Name)(implicit ctx: PrintingContext): Fragment = default(tree)
    def CompoundTypeTree(tree: CompoundTypeTree, tpl: Template)(implicit ctx: PrintingContext): Fragment = default(tree)
    def AppliedTypeTree(tree: AppliedTypeTree, tpt: Tree, args: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def TypeBoundsTree(tree: TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def ExistentialTypeTree(tree: ExistentialTypeTree, tpt: Tree, whereClauses: List[Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def SelectFromArray(tree: SelectFromArray, qualifier: Tree, selector: Name, erasure: Type)(implicit ctx: PrintingContext): Fragment = default(tree)
    // own trees
    def SuperConstructorCall(tree: SuperConstructorCall, clazz: global.Tree, args: List[global.Tree])(implicit ctx: PrintingContext): Fragment = default(tree)
    def ModifierTree(tree: ModifierTree, flag: Long)(implicit ctx: PrintingContext): Fragment = default(tree)
    def MultipleAssignment(tree: MultipleAssignment, extractor: Tree, values: List[ValDef], rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def ImportSelectorTree(tree: ImportSelectorTree, name: NameTree, rename: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def SelfTypeTree(tree: SelfTypeTree, name: NameTree, tpt: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def SourceLayoutTree(tree: SourceLayoutTree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def NameTree(tree: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)
    def NamedArgument(tree: Tree, name: NameTree, rhs: Tree)(implicit ctx: PrintingContext): Fragment = default(tree)

    def printIndentedSingleTree(
      tree: Tree,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

      /*
       * If possible, indent to the same length as the first child of the tree, and
       * if we cannot get an indentation from the children, we use the default increment.
       * */
      printSingleTree(tree, before, after)(ctx.copy(ind = ctx.ind.incrementDefault))
    }

    def p(
      tree: Tree,
      before: Requisite = NoRequisite,
      after: Requisite = NoRequisite)(implicit ctx: PrintingContext)
      = printSingleTree(tree, before, after)

    def pi(
      tree: Tree,
      before: Requisite = NoRequisite,
      after: Requisite = NoRequisite)(implicit ctx: PrintingContext)
      = printIndentedSingleTree(tree, before, after)

    def pp(
      trees: List[Tree],
      separator: Requisite = NoRequisite,
      before: Requisite = NoRequisite,
      after: Requisite = NoRequisite)(implicit ctx: PrintingContext)
      = printManyTrees(trees, separator, before, after)

    def ppi(
      trees: List[Tree],
      before: Requisite = NoRequisite,
      separator: Requisite = NoRequisite,
      after: Requisite = NoRequisite)(implicit ctx: PrintingContext)
      = printIndentedManyTrees(trees, separator, before, after)

    def printSingleTree(
      tree: Tree,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

      import ctx._

      val newIndent = ind.setTo(getChildrenIndentation(parent, tree) getOrElse ind.current)

      val newCtx = ctx copy (ind = newIndent)

      print(tree, newCtx) ifNotEmpty (_ ++ (after, before))
    }

    def printIndentedManyTrees(
      trees: List[Tree],
      separator: Requisite,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

      import ctx._

      val fixedIndentationSeparator = {
        if (parent.hasExistingCode && (
            separator.getLayout.asText.startsWith("\n") ||
            separator.getLayout.asText.startsWith("\r")
            )) {
          Requisite.newline(ind.current + ind.defaultIncrement, ctx.newline)
        } else {
          separator
        }
      }

      trees.map { t =>
        t -> printIndentedSingleTree(t, NoRequisite, NoRequisite)
      }.filter(_._2.asText != "").zipWithIndex.map {

        case ((tree, fragment), 0)
            if tree.pos == NoPosition && separator.getLayout.contains("\n") && parent.isInstanceOf[Template] =>

          if(trees.size > 1 && trees(trees.indexOf(tree) + 1).pos.isRange ) {
            fragment ++ Layout(separator.getLayout.asText)
          } else {
            fragment
          }

        case ((_, fragment), 0) =>
          fragment

        case ((tree, fragment), _) if tree.pos.isRange =>
          fragment

        case ((_, fragment), _)
            if separator.getLayout.contains("\n") && parent.isInstanceOf[Template] =>
          Layout(ctx.newline + fixedIndentationSeparator.getLayout.asText) ++ fragment

        case ((_, fragment), _) =>
          fragment

      }.foldRight(EmptyFragment: Fragment) {
        case (l, r) if l.asText == "" => r
        case (l, r) if r.asText == "" => l
        case (l, r) =>

          val lr = l.post(l.center ++ l.trailing, NoLayout)
          val rr = r.pre(NoLayout, r.leading ++ r.center)
          val mid: Layout = (lr ++ fixedIndentationSeparator ++ rr).toLayout
          Fragment(l.leading, mid, r.trailing) ++ (r.post, l.pre)
      } ifNotEmpty { f =>
        if (parent.hasExistingCode && !trees.head.hasExistingCode && (
                separator.getLayout.asText.startsWith("\n") ||
                separator.getLayout.asText.startsWith("\r")
                )) {
          (Layout(ind.defaultIncrement) ++ f) ++ (after, before)
        } else {
          f ++ (after, before)
        }
      }
    }

    def printManyTrees(
      trees: List[Tree],
      separator: Requisite,
      before: Requisite,
      after: Requisite)(implicit ctx: PrintingContext): Fragment = {

      /*
       * We need to catch match errors because internally this function calls
       * [[scala.reflect.api.Trees.xtraverse(Traverser, Tree)]], a function that
       * we can't easily override for our own trees in
       * [[scala.tools.refactoring.common.PimpedTrees]].
       */
      def isErroneous(t: Tree) =
        try t.isErroneous || t.exists(_.isErroneous)
        catch { case _: MatchError => false }

      val fragment = trees.foldRight(EmptyFragment: Fragment) {
        case (t, r) =>
          printSingleTree(t, NoRequisite, NoRequisite) match {
            case l if l.isEmpty || l.asText.isEmpty => r
            case l if r.asText.isEmpty => l
            case l =>
              val leftCenter =
                if (isErroneous(t)) l.center
                else balanceBracketsInLayout('(', ')', l.center)
              val rightCenter = balanceBracketsInLayout('(', ')', r.center)

              val left = l.post(leftCenter ++ l.trailing, NoLayout)
              val right = r.pre(NoLayout, r.leading ++ rightCenter)
              val middle = (left ++ separator ++ right).toLayout
              Fragment(l.leading, middle, r.trailing) ++ (r.post, l.pre)
           }
      }
      fragment ifNotEmpty (_ ++ (after, before))
    }

    def getChildrenIndentation(parent: Tree, t: Tree): Option[String] = {
      if (parent.hasExistingCode) {

        val childrenOnNewLine = children(parent) filter (_.pos.isRange) filter (_.pos.line != parent.pos.line)

        if (childrenOnNewLine exists (_ samePos t)) {
          Some(indentationString(t))
        } else {
          childrenOnNewLine.headOption map indentationString
        }

      } else None
    }
  }
}
