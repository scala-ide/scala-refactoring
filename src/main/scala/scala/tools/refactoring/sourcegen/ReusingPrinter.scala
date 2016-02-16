/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.tools.refactoring.sourcegen.Layout.LayoutFromFile
import scala.tools.refactoring.util.SourceWithMarker
import scala.tools.refactoring.util.SourceWithMarker.Movement
import scala.tools.refactoring.util.SourceWithMarker.Movements
import scala.tools.refactoring.util.SourceWithMarker.Movements.charToMovement
import scala.tools.refactoring.util.SourceWithMarker.Movements.commentsAndSpaces
import scala.tools.refactoring.util.SourceWithMarker.Movements.space

trait ReusingPrinter extends TreePrintingTraversals with AbstractPrinter with ScalaVersionAdapters.CompilerApiAdapters {

  outer: LayoutHelper with common.Tracing with common.PimpedTrees with common.CompilerAccess with Formatting with Indentations =>

  import global._

  object reusingPrinter extends TreePrinting with PrintingUtils
    with MiscPrinters
    with MethodCallPrinters
    with WhilePrinters
    with PatternMatchingPrinters
    with TypePrinters
    with FunctionPrinters
    with ImportPrinters
    with PackagePrinters
    with TryThrowPrinters
    with ClassModulePrinters
    with IfPrinters
    with ValDefDefPrinters
    with SuperPrinters
    with BlockPrinters
    with LiteralPrinters {

    override def dispatchToPrinter(t: Tree, ctx: PrintingContext): Fragment = {

      val originalIndentation = outer.indentationString(t)

      val newCtx = ctx.copy(ind = ctx.ind.setTo(originalIndentation))

      val (leadingParent, trailingParent) = surroundingLayoutFromParentsAndSiblings(t)

      val printedFragment = if (ctx.changeSet.hasChanged(t)) {
        super.dispatchToPrinter(t, newCtx)
      } else if (t.pos.isTransparent) {
        trace("Not in change set but transparent, continue printing...")
        /*
         * If we have a position that is not in the changeset, we can stop printing
         * and just use the existing source code. But there are potentially many
         * trees with the same transparent position besides a non-transparent range,
         * so we need to look further until we find that non-transparent range and
         * can take its source code.
         *
         * */
        super.dispatchToPrinter(t, newCtx)
      } else {
        trace("Not in change set, keep original code.")
        val end = endPositionAtEndOfSourceFile(t.pos)
        val start = adjustedStartPosForSourceExtraction(t, t.pos).start
        Fragment(t.pos.source.content.slice(start, end).mkString)
      }

      val indentedFragment = {
        if (ctx.ind.needsToBeFixed(originalIndentation, leadingParent, l(newCtx), r(newCtx), trailingParent)) {
          val indentedLeadingLayout = ctx.ind.fixIndentation(leadingParent.asText, originalIndentation)
          val indentedCode = ctx.ind.fixIndentation(printedFragment.asText, originalIndentation)
          Fragment(indentedLeadingLayout, indentedCode, trailingParent)
        } else {
          Fragment(leadingParent, printedFragment.toLayout, trailingParent)
        }
      } \\ { res =>
        trace("Result " + getSimpleClassName(t) + ": %s", res)
      }

      indentedFragment
    }
  }

  trait PrintingUtils {
    this: TreePrinting =>

    implicit def allowSurroundingWhitespace(s: String) = Requisite.allowSurroundingWhitespace(s)

    def l(implicit ctx: PrintingContext) = leadingLayoutForTree(ctx.parent)

    def r(implicit ctx: PrintingContext) = trailingLayoutForTree(ctx.parent)

    def orig(tree: Tree): Tree = findOriginalTree(tree) getOrElse {
      trace("Original tree not found for %s, returning EmptyTree.", tree)
      EmptyTree
    }

    /** Returns a NameTree for a tree's name and gives it the position of
     *  the original tree's name.
     */
    def nameOf(tree: Tree): NameTree = {
      val namePos = orig(tree).namePosition
      outer.NameTree(tree.nameString) setPos namePos
    }

    /** Prints the children of the tree, surrounded with the layout from
     *  the existing code.
     */
    def printChildren(tree: Tree)(implicit ctx: PrintingContext) = {
      l ++ children(tree).foldLeft(EmptyFragment: Fragment)(_ ++ p(_)) ++ r
    }

    /** This is the default handler that is called for non-overriden methods.
     */
    override def default(tree: Tree)(implicit ctx: PrintingContext): Fragment = {
      printChildren(tree)
    }

    def printTemplate(t: Template, printExtends: Boolean)(implicit ctx: PrintingContext) = {

      val TemplateExtractor(params, earlyBody, parents, self, body) = t

      val preBody = {
        val xtends = Requisite.anywhere("extends", " extends ")
        val parents_ = pp(parents, before = if (printExtends) xtends else "", separator = " with ")

        val params_ = params.headOption map (pms => pp(pms, separator = ", ", after = Requisite.anywhere(")"))) getOrElse EmptyFragment
        val SplitAtOpeningBrace = "(.*?)(\\s?\\{.*)".r
        val hasNoClassParameters = params == Nil :: Nil || params == Nil

        if (hasNoClassParameters) {
          l.asText match {
            case SplitAtOpeningBrace(before, after) =>
              Layout(before) ++ pp(earlyBody) ++ parents_.ifNotEmpty(_ ++ Requisite.Blank) ++ Layout(after)
            case _ =>
              pp(earlyBody) ++ l ++ parents_
          }
        } else {
          params_.trailing.asText match {
            case SplitAtOpeningBrace(before, after) if earlyBody.isEmpty =>
              l ++ params_.dropTrailingLayout ++ Layout(before) ++ parents_ ++ Layout(after)
            case _ =>
              l ++ params_ ++ pp(earlyBody) ++ parents_
          }
        }
      }

      def hasNewlyIntroducedBody = orig(t) match {
        case TemplateExtractor(_, _, _, origSelf, origBody) =>
          origBody.isEmpty && isEmptyTree(origSelf) && !body.isEmpty
        case _ => false
      }

      def isExistingBodyAllOnOneLine = {
        val tplStartLine = t.pos.source.offsetToLine(t.pos.start)
        val tplEndLine = t.pos.source.offsetToLine(t.pos.end)
        tplStartLine == tplEndLine
      }

      if (hasNewlyIntroducedBody) {
        val openingBrace = " {" + ctx.newline + indentation
        val closingBrace = ctx.newline + indentation + "}"
        val bodyResult = ppi(body, separator = newline)

        preBody ++ p(self) ++ openingBrace ++ bodyResult ++ closingBrace
      } else if (isExistingBodyAllOnOneLine) {
        preBody ++ p(self) ++ ppi(body, separator = newline) ++ r
      } else {
        val body_ = ppi(body, separator = newline)
        val trailing = r
        val hasOpeningBrace = preBody.asText.contains("{") || body_.leading.contains("{") || trailing.contains("{")
        val needToPrintOpeningBrace = !hasOpeningBrace && trailing.contains("}") && !preBody.asText.endsWith("{")

        /*
         * In some cases, (see for example ExplicitGettersSettersTest.oneVarFromMany() or
         * IndividualSourceGenTest.addMethodToEmptyTraitWithSelfTypeAnnotation()), it is necessary, it insert
         * an extra newline after 'self'. Unfortunately, finding out when an extra newline is needed exactly
         * is not easy, and the solution that you find below is nothing but a hack, that happens to satisfy
         * our test suite. It replaces an earlier hack, that was causing #1002643. The reason for this bug was,
         * that library wrongly inserted a newline after the ')', which then caused ')' not to be matched by
         * preceding requisites, resulting in a surplus ')' being added after the newline.
         *
         * Note that we could remove all the complicated logic below and replace it with
         * 'p(self)' if we were only interested in 'Rename', 'Move class' and
         * 'Organize Imports' (just replace printSelfConsideringTrailingNewline with p(self)
         * to see the tests that fail without this hack).
         */
        def printSelfConsideringTrailingNewline = {
          def mvntToCheckIfNewlineAfterSelfIsNeeded(beforeNewline: Boolean) = {
            import Movements._
            val commentsAndSpacesNoNewline = (character(' ') | '\t' | comment).zeroOrMore

            if (beforeNewline) {
              ')'.optional ~ commentsAndSpaces ~ '{'.optional ~
                (commentsAndSpaces ~ Movements.string("=>")).optional ~
                commentsAndSpacesNoNewline.zeroOrMore ~ '\n'
            } else {
              ('\n' ~ commentsAndSpacesNoNewline).backward
            }
          }

          val needNewlineAfterSelf = {
            val posWithMvntToCheck = self.pos match {
              case rp: RangePosition =>
                Some((rp.end, mvntToCheckIfNewlineAfterSelfIsNeeded(beforeNewline = true)))
              case _ => body.map(_.pos).collectFirst {
                case rp: RangePosition =>
                  (rp.start - 1, mvntToCheckIfNewlineAfterSelfIsNeeded(beforeNewline = false))
              }.orElse {
                (params.flatten ++ earlyBody ++ parents).reverse.map(_.pos).collectFirst {
                  case rp: RangePosition =>
                    (rp.end, mvntToCheckIfNewlineAfterSelfIsNeeded(beforeNewline = true))
                }
              }
            }

            posWithMvntToCheck.map { case (pos, mvnToCheck) =>
              val srcAfterSelf = SourceWithMarker(t.pos.source.content, pos)
              mvnToCheck(srcAfterSelf).isDefined
            }.getOrElse(true)
          }

          if (needToPrintOpeningBrace) {
            EmptyFragment ++ " {" ++ p(self) ++ indentedNewline
          } else if (needNewlineAfterSelf) {
            val preliminaryRes = p(self)
            val newlineAlreadyPresent = {
              preliminaryRes.trailing.asText.contains("\n") ||
                body_.leading.asText.contains("\n") ||
                (body_.isEmpty && trailing.asText.contains("\n"))
            }

            if (newlineAlreadyPresent) preliminaryRes
            else preliminaryRes ++ indentedNewline
          } else {
            p(self)
          }
        }

        val self_ = printSelfConsideringTrailingNewline
        preBody ++ self_ ++ body_ ++ trailing
      }
    }
  }

  trait WhilePrinters {
    this: TreePrinting with PrintingUtils =>

    override def LabelDef(tree: LabelDef, name: Name, params: List[Tree], rhs: Tree)(implicit ctx: PrintingContext) = {

      val labelName = nameOf(tree)

      rhs match {
        case Block(stats, If(cond, _, _)) =>
          l ++ pp(stats) ++ p(labelName) ++ Layout("(") ++ p(cond) ++ r

        case If(cond, Block((body: Block) :: Nil, _), _) =>
          l ++ p(labelName) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ p(body) ++ r

        case If(cond, ifTrue, _) =>
          l ++ p(labelName) ++ Layout("(") ++ p(cond) ++ Layout(")") ++ pi(ifTrue) ++ r
      }
    }
  }

  trait PatternMatchingPrinters {
    this: TreePrinting with PrintingUtils =>

    override def CaseDef(tree: CaseDef, pat: Tree, guard: Tree, body: Tree)(implicit ctx: PrintingContext) = {
      val arrowReq = new Requisite {
        def isRequired(l: Layout, r: Layout) = {
          !(l.contains("=>") || r.contains("=>") || p(body).asText.startsWith("=>"))
        }

        // It's just nice to have a whitespace before and after the arrow
        def getLayout = Layout(" => ")
      }

      val ifReq = new Requisite {
        def isRequired(l: Layout, r: Layout) = {
          !(l.contains("if") || r.contains("if"))
        }

        // Leading and trailing whitespace is required in some cases!
        // e.g. `case i if i > 0 => ???` becomes `case iifi > 0 => ???` otherwise
        def getLayout = Layout(" if ")
      }

      body match {

        case b @ BlockExtractor(body) if !b.hasExistingCode =>
          val x = (l ++ p(pat) ++ p(guard)) ++ arrowReq
          x ++ Fragment(ctx.newline + indentation) ++ ppi(body, separator = indentedNewline) ++ r

        case _ if tree.pos.isTransparent =>
          EmptyFragment

        case _ =>
          val patAndGuard =
            if (guard == EmptyTree)
              l ++ p(pat, after = arrowReq)
            else
              l ++ p(pat) ++ p(guard, before = ifReq, after = arrowReq)

          patAndGuard ++ pi(body) ++ r
      }
    }

    override def Alternative(tree: Alternative, trees: List[Tree])(implicit ctx: PrintingContext) = {
      l ++ pp(trees, separator = " | ") ++ r
    }

    override def Bind(tree: Bind, name: Name, body: Tree)(implicit ctx: PrintingContext) = {
      val nameOrig = nameOf(tree)

      body match {

        case body: Bind =>
          l ++ p(nameOrig) ++ p(body, before = "(", after = ")") ++ r

        case body: Typed =>
          l ++ p(nameOrig) ++ p(body) ++ r

        case _ =>
          l ++ p(nameOrig) ++ p(body, before = " @ ") ++ r
      }
    }

    override def UnApply(tree: UnApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {
      l ++ p(fun) ++ pp(args, separator = ",", before = "(", after = ")") ++ r
    }

    override def Match(tree: Match, selector: Tree, cases: List[Tree])(implicit ctx: PrintingContext) = {
      if (keepTree(selector)) {
        l ++ p(selector) ++ " match" ++ pp(cases) ++ r
      } else {
        l ++ pp(cases) ++ r
      }
    }
  }

  trait MethodCallPrinters {
    this: TreePrinting with PrintingUtils =>

    /*
     * This function takes care of properly printing qualifiers for compiler generated artifacts, that have a
     * corresponding qualifier in the original source code. To understand what that means, consider the statement
     *
     *   O().test.meth()
     *
     * where meth has default arguments, like
     *
     *   def meth(j: Int = 0) = j
     *
     * After typing, the compiler transforms the statement from above into something like
     *
     *   <artifact> val qual$1: C = O.apply().test;
     *   <artifact> val x$1: Int = qual$1.meth$default$1;
     *   qual$1.meth(x$1)
     *
     * with 'qual$1' from the last line pointing to the position of the qualifier 'test' in the original source.
     *
     * Note that simply reading the qualifier from the original source would be wrong if exactly this qualifier
     * was renamed. The reason that this doesn't cause problems is that if 'test' is renamed, 'qual$1.meth(x$1)'
     * remains unchanged, and therefore is not printed at all.
     */
    private def printArtifactQualifierFromSource(q: Tree): Fragment = {
      def mkLayout(start: SourceWithMarker, end: SourceWithMarker): Layout = {
        LayoutFromFile(q.pos.source, start.marker, end.marker)
      }

      val srcStartQ = SourceWithMarker(q.pos.source.content, q.pos.point)
      val srcEndQ = srcStartQ.moveMarker(Movements.id)

      val srcStartSep = srcEndQ
      val srcEndSep = srcStartSep.moveMarker(Movements.until(Movements.id))

      Fragment(NoLayout, mkLayout(srcStartQ, srcEndQ), mkLayout(srcStartSep, srcEndSep))
    }

    private def artifactQualifierIndirectlyRepresentedInSource(q: Tree) = q.pos match {
      case _: RangePosition | NoPosition => false
      case pos => q match {
        case _: This => false
        case _ =>
          val sym = q.symbol
          !sym.isOmittablePrefix && isImplementationArtifact(sym)
      }
    }

    override def Select(tree: Select, qualifier: Tree, selector: Name)(implicit ctx: PrintingContext) = {
      lazy val nameOrig = nameOf(tree)

      qualifier match {

        // skip <init> from constructor calls
        case _ if selector == nme.CONSTRUCTOR =>
          l ++ p(qualifier) ++ r

        case _: This if qualifier.pos == NoPosition =>
          l ++ Fragment(tree.symbol.nameString) ++ r

        case _ if (qualifier.pos == NoPosition || tree.pos.sameRange(qualifier.pos))
          && (selector == nme.unapply || selector == nme.apply || selector == nme.unapplySeq) =>
          if (qualifier.pos == NoPosition) {
            l ++ p(qualifier) ++ r
          } else {
            // at the moment, the qualifier incorrectly has a TransParent position, so we remove that
            l ++ p(qualifier setPos (qualifier.pos withPoint qualifier.pos.point)) ++ r
          }

        case _: Apply if selector.toString.startsWith("unary_") =>

          val printedQualifier = p(qualifier)

          if (printedQualifier.asText.contains(" ")) //XXX better check to see if we need to print parens
            l ++ p(nameOrig) ++ "(" ++ printedQualifier ++ ")" ++ r
          else
            l ++ p(nameOrig) ++ printedQualifier ++ r

        case _ if selector.toString.startsWith("unary_") =>
          l ++ p(nameOrig) ++ p(qualifier) ++ r

        case Apply(s @ global.Select(qual, _), Nil) =>

          val betweenQualifierAndName = {
            val name = nameOf(s)
            if (qual.pos.isRange && name.pos.isRange)
              between(qual, name)
            else NoLayout
          }

          val _qualifier = p(qualifier)

          if (!_qualifier.asText.matches("^\\s*\\(.*\\)\\s*") && betweenQualifierAndName.contains(" ")) {
            l ++ "(" ++ _qualifier ++ ")" ++ " " ++ p(nameOrig) ++ r
          } else {
            l ++ _qualifier ++ p(nameOrig) ++ r
          }

        case _ =>

          val _q = {
            // Workaround for SI-5064
            if (tree.pos.sameRange(qualifier.pos) && qualifier.pos.isTransparent)
              EmptyFragment
            else if (artifactQualifierIndirectlyRepresentedInSource(qualifier))
              printArtifactQualifierFromSource(qualifier)
            else
              p(qualifier)
          }
          val _n = {
            // the selector isn't visible in the source,
            // that's the case e.g. in for expressions.
            if (tree.samePos(qualifier) && qualifier.pos.isRange && !qualifier.isInstanceOf[This])
              EmptyFragment
            else p(nameOrig)
          }

          def hasNoSeparator = {
            val between = (_q.trailing ++ _n.leading).asText
            !between.contains(" ") && !between.contains(".")
          }

          def startsWithChar = _q.asText.matches(".*[^ ]$")
          def endsWithChar = _n.asText.matches("^[^ ].*")

          def qualifierHasNoDot = qualifier match {
            case Apply(s @ global.Select(qual, name), _) if s.pos.isRange && qual.pos.isRange =>

              val sn: global.Name = s.name
              val nt = new NameTree(sn).setPos(s.namePosition)

              val b = between(qual, nt)
              !b.contains(".")
            case _ => false
          }

          def hasClosingParensBetweenQualifierAndSelector = {
            qualifier.pos.isRange && nameOrig.pos.isRange && {
              between(qualifier, nameOrig).contains(")")
            }
          }
          if (qualifier.pos.isRange && tree.pos.start < qualifier.pos.start && nameOrig.nameString.endsWith(":")) {
            l ++ _n ++ " " ++ _q ++ r
          } else if (startsWithChar && endsWithChar && hasNoSeparator) {
            l ++ _q ++ (if (!qualifier.isInstanceOf[Select]) " " else ".") ++ _n ++ r
          } else if (qualifierHasNoDot && _n.leading.contains(".")) {
            l ++ "(" ++ _q ++ ")" ++ _n ++ r
          } else if (hasClosingParensBetweenQualifierAndSelector) {
            l ++ "(" ++ _q ++ ")" ++ _n ++ r
          } else {
            l ++ _q ++ _n ++ r
          }
      }
    }

    override def TypeApply(tree: TypeApply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {
      val _fun = fun match {
        /*
         * We have to check if the TypeApply is the generator of a for-comprehension.
         * There are several indicators: equal position of the fun and the TypeApply,
         * the name and if the name is not at the expected position in the source code,
         * then we assume that this is a for comprehension and only print the qualifier
         * of the function.
         * */
        case global.Select(qual, nme) if fun.pos.eq(tree.pos) && fun.pos.isRange && {
          nme == global.nme.foreach || nme == global.nme.map || nme == global.nme.flatMap
        } && {
          val nmeInSource = betweenPointAndEnd(fun).asText
          nmeInSource != nme.toTermName.toString
        } => p(qual)
        case _ => p(fun)
      }

      val _args = pp(args, separator = ", ", before = "[", after = "]")

      l ++ _fun.dropTrailingLayout ++ balanceBrackets('[', ']')(_fun.trailing ++ _args ++ r)
    }

    override def Apply(tree: Apply, fun: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {

      def balanceParensAroundCall(recv: Fragment, args: Fragment) = {
        /*
         * The opening parenthesis could also be trailing the function, if that's
         * the case we include the trailing layout in the balanceParens call.
         */
        if (recv.trailing.contains("(")) {
          val _arg = balanceBrackets('(', ')')(recv.trailing ++ args ++ r)
          l ++ recv.dropTrailingLayout ++ _arg
        } else {
          val _arg = balanceBrackets('(', ')')(args ++ r)
          l ++ recv ++ _arg
        }
      }

      (fun, args) match {

        case (global.Select(select: Select, nme.update), args) if fun.pos == select.pos && args.nonEmpty =>

          args match {
            case arg :: Nil =>
              l ++ p(select) ++ p(arg) ++ r

            case _ =>
              val updateArgs = args.init
              val rhs = args.last
              l ++ p(select) ++ "(" ++ pp(updateArgs, separator = ", ") ++ ")" ++ " = " ++ p(rhs) ++ r
          }

        // handle e.g. a += 1 which is a = (a + 1)
        case (_: Select, ((arg1: Apply) :: _)) if tree.pos.sameRange(arg1.pos) && arg1.pos.isTransparent =>
          l ++ p(fun) ++ between(fun, arg1.args.head) ++ pp(arg1.args) ++ r

        // x :: xs in pattern match:
        case (EmptyTree, ((_: Bind) :: (_: Bind) :: _)) if tree.tpe.toString.contains("::") =>
          l ++ pp(args) ++ r

        case (_, ((_: Bind) :: (_: Bind) :: _)) =>
          val _fun = p(fun)
          val hasInfixExtractor = (l ++ _fun).asText.isEmpty
          val _args = if (hasInfixExtractor) {
            pp(args)
          } else {
            pp(args, before = if (l contains "(") NoRequisite else "(", separator = ", ", after = ")")
          }
          l ++ _fun ++ _args ++ r

        // ifs in for comprehensions:
        case (fun: Select, arg :: Nil) if keepTree(fun.qualifier) && fun.name.toString == "withFilter" =>
          l ++ p(fun.qualifier) ++ " if " ++ p(arg) ++ r

        case (fun: Select, arg :: Nil) if keepTree(fun.qualifier) /*has receiver*/
          || fun.name.toString.endsWith("$eq") /*assigns*/ =>

          balanceParensAroundCall(p(fun), p(arg))

        case (TypeApply(_: Select, _), (arg @ Function(_, _: Match)) :: Nil) =>
          l ++ p(fun) ++ p(arg) ++ r

        case (fun @ TypeApply(receiver: Select, _), NoFunction(arg) :: Nil) if receiver != null && keepTree(fun) =>

          val isReceiverMethodCallWithDot = {
            receiver.qualifier.pos.isRange && betweenEndAndPoint(receiver.qualifier, receiver).contains(".")
          }

          if (isReceiverMethodCallWithDot || !keepTree(receiver.qualifier) || l.contains("(") || r.contains(")")) {
            l ++ p(fun) ++ p(arg, before = Requisite.anywhere("("), after = Requisite.anywhere(")")) ++ r
          } else {
            val arg_ = p(arg)
            if (arg_.asText.matches("""(?ms)\s*\{.*""") && !arg_.asText.matches("""(?ms).*\}\s*""")) {
              l ++ p(fun) ++ arg_ ++ indentedNewline ++ "}" ++ r
            } else {
              l ++ p(fun) ++ arg_ ++ r
            }
          }

        case (fun, arg :: Nil) if !keepTree(fun) =>
          l ++ p(arg) ++ r

        case (EmptyTree, args) =>
          l ++ pp(args, separator = ", ", before = "(", after = ")") ++ r

        /* Workaround for for-comprehensions. Because they are not represented with
         * their own ASTs, we sometimes need to work around some issues. This is for
         * the following case:
         *
         *   for(`arg` <- `fun`) yield body
         *
         * We discover this pattern by the transparent function with a position
         * smaller than the preceding (in the AST) Apply call. */
        case (generator, (f @ Function(arg :: _, body)) :: Nil) if f.pos.isTransparent && generator.pos != NoPosition &&
          arg.pos.startOrPoint < generator.pos.startOrPoint &&
          between(arg, generator).contains("<-") =>

          val src = betweenStartAndPoint(tree)

          val layoutAfterGenerator = {
            /*
             * Handle 3 ways to write for-comprehensions:
             *
             * for {
             *   ...
             * } ...
             *
             * for { ... }
             *
             * for ( ... )
             *
             * */
            if (src.matches("""(?ms).*\{\s*\n.*""")) {
              indentedNewline ++ "}"
            } else if (src.contains("{")) {
              allowSurroundingWhitespace("}")
            } else {
              allowSurroundingWhitespace(")")
            }
          }

          /* We only regenerate the code of the generator and the body, this will fail
           * to pick up any changes in the `arg`!
           *
           * Generic layout handling will remove a closing `)`, so we re-add it */
          val _generator = p(generator, after = layoutAfterGenerator)

          val isUnit = generator match {
            case generator: TypeApply =>
              generator.tpe match {
                case MethodType(_, TypeRef(_, sym, _)) =>
                  sym.name == tpnme.Unit
                case _ => false
              }
            case _ => false
          }

          val bodyPrefix = if (isUnit) " " else " yield "

          if (body.pos.isRange && between(generator, body).matches("""(?ms).*\{\s*$""")) {
            val nextLine = if (body.pos.line > generator.pos.line) {
              ctx.newline + ctx.ind.incrementDefault.current
            } else " "

            l ++ _generator ++ p(body, before = bodyPrefix + "{" + nextLine) ++ r
          } else {
            l ++ _generator ++ p(body, before = bodyPrefix) ++ r
          }

        case (fun, Nil) =>
          val trailingLayout: Layout = {
            val rr = r
            if (rr.nonEmpty) {
              if ((commentsAndSpaces ~ ')').consumes(rr.asText)) {
                // Needed for IndividualSourceGenTest.testPrintParentheses
                Layout("(") ++ rr
              } else {
                rr
              }
            } else {
              fun.pos match {
                case pos: RangePosition =>
                  // If fun is part of an expression involving default arguments, it might be
                  // necessary to manually make sure that parenthesis and '.' is printed out.
                  // See #1002564 and #1002611
                  val cs = commentsAndSpaces
                  val parensAndSep = cs ~ '(' ~ cs ~ ')' ~ (cs ~ '.' ~ cs).optional
                  Layout(Movement.coveredStringStartingAtEndOf(pos, parensAndSep))
                case _ => NoLayout
              }
            }
          }

          l ++ p(fun) ++ trailingLayout

        case (fun, args) if !keepTree(fun) /* Constructors, Tuples */ =>
          val _args = pp(args, separator = ("," ++ Requisite.Blank), before = Requisite.anywhere("("), after = Requisite.anywhere(")"))
          l ++ _args ++ r

        case (fun, args) =>
          val _args = pp(args, separator = ("," ++ Requisite.Blank), before = "(", after = Requisite.anywhere(")"))
          balanceParensAroundCall(p(fun), _args)
      }
    }
  }

  trait TypePrinters {
    this: TreePrinting with PrintingUtils =>

    override def TypeTree(tree: TypeTree)(implicit ctx: PrintingContext) = {
      if (tree.original == null && !tree.pos.isTransparent) {
        tree.tpe match {
          case ref @ RefinedType(_ :: parents, _) =>
            l ++ Fragment(parents mkString " ") ++ r
          case t =>
            l ++ Fragment(t.toString) ++ r
        }

      } else {
        tree.tpe match {
          case typeRef @ TypeRef(tpe, sym, parents) if tree.original == null && definitions.isFunctionType(typeRef) && !parents.isEmpty =>
            l ++ typeToString(tree, typeRef) ++ r
          case _ =>
            l ++ p(tree.original) ++ r
        }
      }
    }

    override def TypeDef(tree: TypeDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], rhs: Tree)(implicit ctx: PrintingContext) = {
      val nameTree = nameOf(tree)
      l ++ pp(mods ::: nameTree :: Nil, separator = Requisite.Blank) ++ pp(tparams, before = "[" /*, after = "]"*/ ) ++ p(rhs) ++ r
    }

    override def SelectFromTypeTree(tree: SelectFromTypeTree, qualifier: Tree, selector: Name)(implicit ctx: PrintingContext) = {
      l ++ p(qualifier) ++ p(nameOf(tree)) ++ r
    }

    override def CompoundTypeTree(tree: CompoundTypeTree, tpl: Template)(implicit ctx: PrintingContext) = {
      balanceBrackets('{', '}') {
        printTemplate(tpl, printExtends = false)
      }
    }

    override def ExistentialTypeTree(tree: ExistentialTypeTree, tpt: Tree, whereClauses: List[Tree])(implicit ctx: PrintingContext) = {
      whereClauses match {
        // [_]
        case (t: TypeDef) :: Nil if t.symbol.isSynthetic =>
          p(tpt) ++ p(t, before = "[", after = "]")

        case _ =>
          p(tpt) ++ pp(whereClauses, before = " forSome {", after = " }")
      }
    }

    override def AppliedTypeTree(tree: AppliedTypeTree, tpt: Tree, args: List[Tree])(implicit ctx: PrintingContext) = {

      def printFunctionType() = {
        if (args.size == 1) {
          l ++ "() => " ++ p(args.head) ++ r
        } else if (args.size == 2) {
          l ++ p(args.head) ++ p(args.last) ++ r
        } else {
          val arguments = args.init
          val ret = args.last
          l ++ pp(arguments, before = "(", separator = ",", after = Requisite.anywhere(")")) ++ p(ret) ++ r
        }
      }

      tpt match {
        case Select(_, tpnme.REPEATED_PARAM_CLASS_NAME | tpnme.BYNAME_PARAM_CLASS_NAME) =>
          l ++ p(args.head) ++ r
        case _ if isEmptyTree(tpt) && args.size == 1 =>
          l ++ p(args.head) ++ r
        case Select(_, name) if name.toString.matches("Function\\d+") =>
          printFunctionType()
        case EmptyTree =>
          printFunctionType()
        case _ =>
          val (beforeArgs, afterArgs) = {
            if (!tpt.pos.isRange) ("", "") //<-- No brackets for tuples like (X, Y) - see #1001932
            else ("[", "]")
          }

          l ++ p(tpt) ++ pp(args, before = beforeArgs, separator = Requisite.allowSurroundingWhitespace(","), after = afterArgs) ++ r
      }
    }

    override def TypeBoundsTree(tree: TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: PrintingContext) = {
      val lo_ = if (lo.pos.isDefined) p(lo) else EmptyFragment
      val hi_ = if (hi.pos.isDefined) p(hi) else EmptyFragment

      lo_ ++ hi_
    }
  }

  trait FunctionPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Function(tree: Function, vparams: List[ValDef], body: Tree)(implicit ctx: PrintingContext) = {
      body match {

        case b @ BlockExtractor(body) if !b.hasExistingCode =>
          l ++ pp(vparams) ++ (ctx.newline + indentation) ++ ppi(body, separator = indentedNewline) ++ r

        case _ =>
          val params = pp(vparams, separator = ", ")

          if (r.contains(")")) {
            l ++ params ++ "(" ++ p(body) ++ r
          } else {
            l ++ params ++ p(body) ++ r
          }
      }
    }
  }

  trait ImportPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Import(tree: Import, expr: Tree, selectors: List[ImportSelectorTree])(implicit ctx: PrintingContext) = {

      val sp = spacingAroundMultipleImports

      val selectors_ = pp(selectors, before = sp, separator = ", ", after = sp)

      if (selectors.size > 1) {
        l ++ "import " ++ p(expr, after = ".") ++ "{" ++ selectors_ ++ "}" ++ r
      } else {
        l ++ "import " ++ p(expr, after = ".") ++ selectors_ ++ r
      }
    }
  }

  trait PackagePrinters {
    this: TreePrinting with PrintingUtils =>

    override def PackageDef(tree: PackageDef, pid: RefTree, stats: List[Tree])(implicit ctx: PrintingContext) = {

      val originalPackageDef = findOriginalTree(tree)

      def isPackageObjectWithNoTopLevelImports = originalPackageDef exists {
        case global.PackageDef(_, ModuleDef(_, nme.PACKAGEkw, _) :: Nil) => true
        case _ => false
      }

      val (importStats, rest) = stats.span(_.isInstanceOf[Import])

      val imports = importStats.collect {
        case t @ Import(expr, _) if expr != EmptyTree => t
      }

      val restStats = rest flatMap {
        case global.PackageDef(pid, stats) if pid.name == nme.EMPTY_PACKAGE_NAME => stats
        case t => List(t)
      }

      def hasOnlyNewImports = !imports.isEmpty && !imports.exists(_.pos.isRange)

      val pid_ = {
        // default package:
        if (pid.name == nme.EMPTY_PACKAGE_NAME) {
          EmptyFragment
        } else {
          l ++ p(pid, before = "package" ++ Requisite.Blank)
        }
      }

      if (isPackageObjectWithNoTopLevelImports) {
        pp(imports, separator = indentedNewline, after = indentedNewline) ++ pid_ ++ pp(restStats, separator = indentedNewline) ++ r
      } else {
        val imports_ = if (hasOnlyNewImports) {
          pp(imports, separator = indentedNewline) ++ newline ++ indentedNewline
        } else {
          pp(imports, separator = indentedNewline)
        }

        pid_.ifNotEmpty(_ ++ newline ++ indentedNewline) ++ imports_ ++ pp(restStats, separator = newline ++ indentedNewline) ++ r
      }
    }
  }

  trait TryThrowPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Try(tree: Try, block: Tree, catches: List[Tree], finalizer: Tree)(implicit ctx: PrintingContext) = {
      block match {

        case b @ BlockExtractor(block) if !b.hasExistingCode =>
          l ++ indentation ++ ppi(block, separator = indentedNewline) ++ pp(catches) ++ p(finalizer) ++ r

        case _ =>
          printChildren(tree)
      }
    }
  }

  trait ClassModulePrinters {
    this: TreePrinting with PrintingUtils =>

    override def ClassDef(tree: ClassDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], impl: Template)(implicit ctx: PrintingContext) = {
      val className = if (tree.symbol.isAnonymousClass)
        EmptyFragment
      else
        p(nameOf(tree))

      val modifiers = pp(mods, separator = Requisite.Blank, after = Requisite.Blank)
      val typeParams = pp(tparams, separator = "," ++ Requisite.Blank, before = "[", after = "]")
      val template = p(impl)

      val beforeTpl = l ++ modifiers ++ className ++ typeParams

      if (beforeTpl.asText.endsWith(" ") && template.asText.startsWith(" ")) {
        beforeTpl ++ Layout(template.asText.tail)
      } else {
        beforeTpl ++ template
      } ++ r
    }

    override def ModuleDef(tree: ModuleDef, mods: List[ModifierTree], name: Name, impl: Template)(implicit ctx: PrintingContext) = {
      val nameTree = p(nameOf(tree))
      val impl_ = p(impl)
      if (nameTree.asText.endsWith(" ") && impl_.asText.startsWith(" ")) {
        l ++ pp(mods) ++ Layout(nameTree.asText.init) ++ impl_ ++ r
      } else {
        l ++ pp(mods) ++ nameTree ++ impl_ ++ r
      }
    }

    override def Template(tree: Template, parents: List[Tree], self: Tree, body: List[Tree])(implicit ctx: PrintingContext) = {
      printTemplate(tree, printExtends = !tree.isTemplateForAnonymousClass)
    }
  }

  trait IfPrinters {
    this: TreePrinting with PrintingUtils =>

    override def If(tree: If, cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: PrintingContext) = {

      val o = orig(tree).asInstanceOf[If]

      val _else = {

        /*
         * Printing the else branch is tricky because of how {} are handled in the AST,
         * but only if the else branch already existed:
         */
        val elseBranchAlreadyExisted = keepTree(o.elsep) && o.elsep.pos.isRange

        if (elseBranchAlreadyExisted) {

          val layout = between(o.thenp, o.elsep).asText
          val l = Requisite.anywhere(layout.replaceAll("(?ms)else\\s*?\r?\n\\s*$", "else "))

          val curlyBracesAlreadyExist = layout.contains("{")
          val originalElseHasNoBlock = !o.elsep.isInstanceOf[Block]

          elsep match {

            /*
             * The existing else branch was enclosed by {} but contained only a single
             * statement.
             * */
            case BlockExtractor(body) if originalElseHasNoBlock && curlyBracesAlreadyExist =>
              pp(body, before = l, separator = Requisite.newline(ctx.ind.current + ctx.ind.defaultIncrement, ctx.newline))

            /*
             * If there was no block before and also no curly braces, we have to write
             * them now (indirectly through the Block), but we don't want to add any
             * indentation.
             * */
            case elsep: Block =>
              outer.print(elsep, ctx) ifNotEmpty (_ ++ (NoRequisite, l))

            /* If it's a single statements, we print it indented: */
            case _ =>
              pi(elsep, before = Requisite.anywhere(layout))
          }

        } else {
          val l = indentedNewline ++ "else" ++ Requisite.newline(ctx.ind.current + ctx.ind.defaultIncrement, ctx.newline)
          pi(elsep, before = l)
        }
      }

      val (_thenLeadingLayout, _then) = {
        thenp match {
          case block: Block =>
            p(block)
          case _ if keepTree(o.thenp) && o.thenp.pos.isRange =>
            val layout = between(o.cond, o.thenp).asText
            val printedThen = pi(thenp)

            if (layout.contains("{") && !printedThen.asText.matches("(?ms)^\\s*\\{.*")) {
              val (_, right) = layout.splitAt(layout.indexOf(")") + 1)
              pi(thenp, before = Requisite.anywhere(right))
            } else {
              pi(thenp)
            }

          case _ =>
            pi(thenp)
        }
      } match {
        case f => (f.leading, f.dropLeadingLayout)
      }

      val _cond = balanceBrackets('(', ')') {
        // we want to balance the parens around the condition and all adjacent layout
        l ++ p(cond, before = "(", after = Requisite.anywhere(")")) ++ _thenLeadingLayout
      }

      val condAndThenOnSameLine = (cond.pos, thenp.pos) match {
        case (NoPosition, _) => false
        case (_, NoPosition) => true
        case (p1, p2) => p1.line == p2.line
      }

      val hasSeparatorBetweenCondAndThen = {
        _then.asText.startsWith(" ") || _cond.asText.endsWith(" ")
      }

      if (condAndThenOnSameLine && !hasSeparatorBetweenCondAndThen) {
        _cond ++ " " ++ _then ++ _else ++ r
      } else {
        _cond ++ _then ++ _else ++ r
      }
    }
  }

  trait ValDefDefPrinters {
    this: TreePrinting with PrintingUtils =>

    override def ValDef(tree: ValDef, mods: List[ModifierTree], name: Name, tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
      val nameTree = nameOf(tree)

      val modsAndName = rhs match {
        // It looks like we're in a "multiple assignment", then we don't print
        // the modifiers to avoid getting val (val x, val y) = ...
        case Select(qual, _) if qual.symbol.isSynthetic =>
          nameTree :: Nil
        case _ =>
          mods ::: nameTree :: Nil
      }

      // Handle right-associate methods, where there's a synthetic value that holds
      // the argument that gets passed. Strange, but seems to work..
      if (tree.symbol.isSynthetic && (!tree.pos.includes(rhs.pos) || tree.pos.point > tree.pos.end)) {
        p(tpt) ++ p(rhs) ++ r
      } else {
        val mods_ = pp(modsAndName, separator = Requisite.Blank)
        val resultType = p(tpt, before = Requisite.allowSurroundingWhitespace(":", ": "))
        val body = p(rhs)
        val noEqualNeeded = body == EmptyFragment || rhs.tpe == null || (rhs.tpe != null && rhs.tpe.toString == "Unit")

        if (noEqualNeeded)
          l ++ mods_ ++ resultType ++ body ++ r
        else
          l ++ mods_ ++ resultType ++ Requisite.anywhere("=", " = ") ++ body ++ r
      }
    }

    override def DefDef(tree: DefDef, mods: List[ModifierTree], name: Name, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
      val nameTree = nameOf(tree)
      val modsAndName = pp(mods ::: nameTree :: Nil, separator = Requisite.Blank)

      val parameters = {
        // The `)` is always removed from the layout, so if we have an empty
        // parameter list and `()` in the source, we need to insert it here.
        def modsAndNameEndsWithOpenParen = {
          val srcAtEnd = SourceWithMarker(modsAndName.asText).withMarkerAtLastChar
          srcAtEnd.moveMarkerBack(commentsAndSpaces).currentOption == Some('(')
        }

        if (vparamss == List(List()) && modsAndNameEndsWithOpenParen) {
          Fragment(")")
        } else {
          tree.explicitVParamss.map { vparams =>
            pp(vparams, before = "(", separator = ", ", after = ")")
          }.foldLeft(EmptyFragment: Fragment)(_ ++ _)
        }
      }

      val typeParameters = {

        def mergeTypeParameters(ts: List[Tree]): Fragment = ts match {
          case Nil =>
            EmptyFragment
          case (x: TypeDef) :: Nil =>
            p(x)
          case (x: TypeDef) :: (y: TypeDef) :: rest =>
            p(x) ++ "," ++ mergeTypeParameters(y :: rest)
          case (x: TypeDef) :: rest =>
            val (bounds, next) = rest.span(!_.isInstanceOf[TypeDef])
            val current = pp(x :: bounds, separator = ": ")
            if (next.isEmpty) {
              current
            } else {
              current ++ "," ++ mergeTypeParameters(next)
            }
          case _ =>
            EmptyFragment
        }

        val _tparams = mergeTypeParameters(tree.tparamsWithContextBounds) ifNotEmpty {
          _ ++ (before = "[", after = Requisite.anywhere("]"))
        }

        if (parameters.isEmpty && !_tparams.isEmpty && _tparams.trailing.contains("(")) {
          _tparams.toLayout ++ Requisite.anywhere(")")
        } else {
          _tparams
        }
      }

      val body = p(rhs)

      def existsTptInFile = tpt match {
        case tpt: TypeTree =>
          def tpeInFileIsUnit = betweenStartAndEnd(tpt).asText == "Unit"
          def tpeIsUnit = tpt.toString == "Unit"

          // This check handles the special case where a method with a non unit return type
          // is overriden by a method without a return type. This is not legal Scala but can
          // be useful to implement transformations for non-compiling code.
          // See ReusingPrinterTest.add_modifier_to_def_without_return_type for a test case.
          def tpeIsntFromOverrideWithMissingRetType = (tpeInFileIsUnit == tpeIsUnit)

          tpt.pos.isRange && tpeIsntFromOverrideWithMissingRetType

        case _ => false
      }

      val resultType =
        if (body == EmptyFragment && !existsTptInFile)
          EmptyFragment
        else
          p(tpt, before = Requisite.allowSurroundingWhitespace(":", ": "))

      def hasEqualInSource = {
        val originalDefDef = orig(tree)
        (originalDefDef :: children(originalDefDef)).filter(_.pos.isRange).reverse match {
          case last :: secondlast :: _ =>
            between(secondlast, last).contains("=")
          case _ => false
        }
      }

      val noEqualNeeded = {
        body == EmptyFragment || rhs.tpe == null || (rhs.tpe != null && rhs.tpe.toString == "Unit")
      }

      if (noEqualNeeded && !hasEqualInSource) {
        l ++ modsAndName ++ typeParameters ++ parameters ++ resultType ++ body ++ r
      } else {
        l ++ modsAndName ++ typeParameters ++ parameters ++ resultType ++ Requisite.anywhere("=", " = ") ++ body ++ r
      }
    }
  }

  trait SuperPrinters {
    this: TreePrinting with PrintingUtils =>

    override def SuperConstructorCall(tree: SuperConstructorCall, clazz: global.Tree, args: List[global.Tree])(implicit ctx: PrintingContext) = {
      l ++ p(clazz) ++ pp(args, separator = ",", before = "(", after = ")") ++ r
    }

    override def Super(tree: Super, qual: Tree, mix: Name)(implicit ctx: PrintingContext) = {

      // duplicate of pretty printer!
      val q = qual match {
        case This(qual: Name) if qual.toString == "" => EmptyFragment
        case This(qual: Name) => Fragment(qual.toString + ".")
        case _ => p(qual)
      }

      val m = if (mix.toString == "") "" else "[" + mix + "]"

      l ++ q ++ Fragment("super" + m) ++ r

    }
  }

  trait LiteralPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Literal(tree: Literal, value: Constant)(implicit ctx: PrintingContext) = {
      if (value.tag == StringTag) {
        val escaped = value.stringValue.replace("""\""", """\\""")
        l ++ Fragment("\"" + escaped + "\"") ++ r
      } else if (value.isNumeric) {
        Fragment((l ++ betweenStartAndEnd(tree) ++ r).asText)
      } else if (charAtTreeStartPos(tree) == Some('{') && charBeforeTreeEndPos(tree) == Some('}')) {
        /*
         * Scala 2.9:
         *
         * Empty RHS of DefDefs are Literals
         * */
        trace("Literal tree is empty { }")
        Fragment((l ++ betweenStartAndEnd(tree) ++ r).asText)
      } else if (isClassTag(value)) {
        val tpe = value.tpe match {
          case TypeRef(_, _, arg :: Nil) =>
            arg
          case tpe =>
            tpe.toString
        }
        l ++ Fragment("classOf[" + tpe.toString + "]") ++ r
      } else {
        l ++ Fragment(value.stringValue) ++ r
      }
    }

    def charAtTreeStartPos(t: Tree) = t.pos match {
      case range: RangePosition => Some(t.pos.source.content(t.pos.start))
      case _ => None
    }

    def charBeforeTreeEndPos(t: Tree) = t.pos match {
      case range: RangePosition => Some(t.pos.source.content(t.pos.end - 1))
      case _ => None
    }
  }

  trait BlockPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Block(tree: Block, stats: List[Tree])(implicit ctx: PrintingContext) = {

      def allTreesOnSameLine(ts: List[Tree]): Boolean = {
        val poss = ts map (_.pos)
        poss.forall(_.isRange) && (poss.map(_.line).distinct.length <= 1)
      }

      if (stats.size > 1 && allTreesOnSameLine(stats)) {
        l ++ pp(stats) ++ r
      } else {
        if (l.contains("{") && !stats.head.hasExistingCode) {
          val rest = ppi(stats, separator = newline) ++ r
          l ++ Requisite.newline(ctx.ind.current, ctx.newline, force = true) ++ rest

        } else if (stats.size == 2 && !stats(0).hasExistingCode && stats(1).hasExistingCode) {
          val stats_ = pp(stats, separator = indentedNewline)
          l ++ stats_ ++ r
        } else {
          l ++ ppi(stats, separator = newline) ++ r
        }
      }
    }
  }

  trait MiscPrinters {
    this: TreePrinting with PrintingUtils =>

    override def Assign(tree: Assign, lhs: Tree, rhs: Tree)(implicit ctx: PrintingContext) = {
      rhs match {
        // Handle assignments like +=, which are desugared in the AST
        case Apply(Select(_, name), _) if rhs.pos.isTransparent && name.isOperatorName =>
          // the rhs already contains the layout for the operator, note that this might
          // break should the refactoring change the operator name as well.
          l ++ p(rhs) ++ r
        case _ =>
          l ++ p(lhs, after = "=") ++ p(rhs) ++ r
      }
    }

    override def MultipleAssignment(tree: MultipleAssignment, extractor: Tree, values: List[ValDef], rhs: Tree)(implicit ctx: PrintingContext) = {
      extractor match {
        case EmptyTree =>
          l ++ pp(values, separator = ", ") ++ ")" ++ p(rhs) ++ r
        case _ =>
          l ++ "val " ++ p(extractor) ++ " = " ++ p(rhs) ++ r
      }
    }

    override def New(tree: New, tpt: Tree)(implicit ctx: PrintingContext) = {
      if (tree.pos.start > tree.pos.point) {
        Fragment("new ") ++ l ++ p(tpt) ++ r
      } else {
        Fragment("new") ++ l ++ p(tpt) ++ r
      }
    }

    override def This(tree: This, qual: Name)(implicit ctx: PrintingContext) = {
      l ++ Fragment((if (qual.toString == "") "" else qual + ".") + "this") ++ r
    }

    override def Ident(tree: Ident, name: Name)(implicit ctx: PrintingContext) = {
      l ++ Fragment(tree.nameString) ++ r
    }

    override def ModifierTree(tree: ModifierTree, flag: Long)(implicit ctx: PrintingContext) = {
      l ++ Fragment(tree.nameString) ++ r
    }

    override def NameTree(tree: Tree)(implicit ctx: PrintingContext) = {
      if (tree.pos.isTransparent) {
        l ++ EmptyFragment ++ r
      } else {
        l ++ Fragment(tree.nameString) ++ r
      }
    }

    override def NamedArgument(tree: Tree, name: NameTree, rhs: Tree)(implicit ctx: PrintingContext) = {
      l ++ p(name) ++ Requisite.Blank ++ "=" ++ Requisite.Blank ++ p(rhs) ++ r
    }
  }
}
