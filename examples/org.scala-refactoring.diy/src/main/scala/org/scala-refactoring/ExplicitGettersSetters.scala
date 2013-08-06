package scala.tools.refactoring

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.ast.parser.Tokens
import scala.swing._
import common.Change
import common.Tracing
import util.CompilerProvider

class MyRefactoring(sourceCode: String, caretPosition: Int) extends Refactoring with Tracing with CompilerProvider {

  import global._

  def refactor(): String = {

    val ast = treeFrom(sourceCode)

    val selection = new FileSelection(ast.pos.source.file, caretPosition, caretPosition+1)

    val selectedValue = selection.findSelectedOfType[ValDef].getOrElse {
      return "No val/var selected."
    }

    val template = selection.findSelectedOfType[Template].getOrElse {
      return "No enclosing class found."
    }

    val createSetter = selectedValue.symbol.isMutable

    val publicName = selectedValue.name.toString.trim

    val privateName = "_"+ publicName

    val privateFieldMods = if(createSetter)
      Modifiers(Flags.PARAMACCESSOR) withPosition (Flags.PRIVATE, NoPosition) withPosition (Tokens.VAR, NoPosition)
    else
      Modifiers(Flags.PARAMACCESSOR)

    val privateField = selectedValue copy (mods = privateFieldMods, name = privateName)

    val getter = DefDef(
        mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition),
        name = publicName,
        tparams = Nil,
        vparamss = List(Nil),
        tpt = EmptyTree,
        rhs = Block(
            Ident(privateName) :: Nil, EmptyTree))

    val setter = DefDef(
        mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition),
        name = publicName +"_=",
        tparams = Nil,
        vparamss = List(List(ValDef(Modifiers(Flags.PARAM), publicName, TypeTree(selectedValue.tpt.tpe), EmptyTree))),
        tpt = EmptyTree,
        rhs = Block(
            Assign(
                Ident(privateName),
                Ident(publicName)) :: Nil, EmptyTree))

    val insertGetterSettersTransformation = transform {

      case tpl: Template if tpl == template =>

        val classParameters = tpl.body.map {
          case t: ValDef if t == selectedValue => privateField setPos t.pos
          case t => t
        }

        val body = if(createSetter)
          getter :: setter :: classParameters
        else
          getter :: classParameters

        tpl.copy(body = body) setPos tpl.pos
    }

    val transformedAst = ↓(matchingChildren(insertGetterSettersTransformation)) apply ast

    val changes = refactor(transformedAst.toList)

    Change.applyChanges(changes, sourceCode)
  }

}

object ExplicitGettersSetters extends SimpleSwingApplication {

  override def main(args: Array[String]): Unit = {

    if(!args.isEmpty) {
      try {
        input.text = scala.io.Source.fromFile(args.head).mkString
      } catch {
        case _: java.io.FileNotFoundException => ()
      }
    }

    super.main(args)
  }

  val input = new TextArea("", 20, 40)

  val button = Button("Refactor!") {
    new MyRefactoring(input.text, input.caret.position) {
      input.text = refactor()
    }
  }

  def top = new MainFrame {
    title = "DIY Refactoring"
    contents = new FlowPanel(input, button)
  }
}
