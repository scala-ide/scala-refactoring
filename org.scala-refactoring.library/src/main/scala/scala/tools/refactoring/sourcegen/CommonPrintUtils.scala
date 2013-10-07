/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package sourcegen

import scala.reflect.internal.util.BatchSourceFile

trait CommonPrintUtils {

  this: common.CompilerAccess with AbstractPrinter =>

  import global._

  def newline(implicit ctx: PrintingContext) = Requisite.newline("", ctx.newline)

  def indentedNewline(implicit ctx: PrintingContext) = Requisite.newline(ctx.ind.current, ctx.newline)

  def newlineIndentedToChildren(implicit ctx: PrintingContext) = Requisite.newline(ctx.ind.incrementDefault.current, ctx.newline)

  def indentation(implicit ctx: PrintingContext) = ctx.ind.current

  def typeToString(tree: TypeTree, t: Type)(implicit ctx: PrintingContext): String = {
    t match {
      case tpe if tpe == EmptyTree.tpe => ""
      case tpe: ConstantType =>
        tpe.typeSymbol.tpe.toString
      case tpe: TypeRef if tree.original != null && tpe.sym.nameString.matches("Tuple\\d+") =>
        tpe.toString
      case tpe if tree.original != null && !tpe.isInstanceOf[TypeRef] =>
        print(tree.original, ctx).asText
      case tpe: RefinedType =>
        tpe.typeSymbol.tpe.toString
      case typeRef @ TypeRef(_, _, arg1 :: ret :: Nil) if definitions.isFunctionType(typeRef) =>
        typeToString(tree, arg1) + " => " + typeToString(tree, ret)
      case MethodType(params, result) =>
        val printedParams = params.map(s => typeToString(tree, s.tpe)).mkString(", ")
        val printedResult = typeToString(tree, result)

        if (params.isEmpty) {
          "() => " + printedResult
        } else if (params.size > 1) {
          "(" + printedParams + ") => " + printedResult
        } else {
          printedParams + " => " + printedResult
        }

      case tpe =>
        tpe.toString
    }
  }

  def balanceParens(open: Char, close: Char)(f: Fragment) = Fragment {
    val txt = f.toLayout.withoutComments // TODO also without strings, etc.
    val opening = txt.count(_ == open)
    val closing = txt.count(_ == close)
    if (opening > closing && closing > 0) {
      f.asText.reverse.replaceFirst("\\" + close, ("" + close) * (opening - closing + 1)).reverse
    } else if (opening > closing) {
      f.asText + (("" + close) * (opening - closing))
    } else if (opening < closing) {
      (("" + open) * (closing - opening)) + f.asText
    } else {
      f.asText
    }
  }

  /**
   * When extracting source code from the file via a tree's position,
   * it depends on the tree type whether we can use the position's
   * start or point.
   *
   * @param t The tree that will be replaced.
   * @param p The position to adapt. This does not have to be the position of t.
   */
  def adjustedStartPosForSourceExtraction(t: Tree, p: Position): Position = t match {
    case _: Select | _: New if t.pos.isRange && t.pos.start > t.pos.point =>
      p withStart (p.start min p.point)
    case _ =>
      p
  }

  lazy val precedence: Name => Int = {

    // Copied from the compiler
    def newUnitParser(code: String)      = new syntaxAnalyzer.UnitParser(newCompilationUnit(code))
    def newCompilationUnit(code: String) = new CompilationUnit(newSourceFile(code))
    def newSourceFile(code: String)      = new BatchSourceFile("<refactoring>", code)

    val parser = newUnitParser("")

    // I ♥ Scala
    name => parser.precedence(newTermName(name.decode))
  }

}