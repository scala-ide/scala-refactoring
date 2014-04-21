package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.common.TextChange

abstract class AddField extends AddValOrDef {

  val global: tools.nsc.interactive.Global
  import global._

  def addField(file: AbstractFile, className: String, valName: String, isVar: Boolean, returnTypeOpt: Option[String], target: AddMethodTarget): List[TextChange] =
    addValOrDef(file, className, target, addField(valName, isVar, returnTypeOpt, _))

  private def addField(valName: String, isVar: Boolean, returnTypeOpt: Option[String], classOrObjectDef: Tree): List[TextChange] = {
    val returnStatement = Ident("???")

    val returnType = returnTypeOpt.map(name => TypeTree(newType(name))).getOrElse(new TypeTree)

    val fieldSym = if (isVar) NoSymbol.newVariable(newTermName(s"${nme.VARkw} " + valName))
      else NoSymbol.newValue(valName)

    val newVal = mkValDef(fieldSym.name.toString, returnStatement, returnType) setSymbol fieldSym

    def addMethodToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: newVal :: Nil) replaces tpl

    val insertMethodCall = transform {
      case ClassDef(_, _, _, tpl) => addMethodToTemplate(tpl)
      case ModuleDef(_, _, tpl) => addMethodToTemplate(tpl)
    }

    refactor((insertMethodCall apply classOrObjectDef).toList)
  }
}