package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.common.TextChange
import scala.tools.nsc.ast.parser.Tokens

abstract class AddField extends AddValOrDef {

  val global: tools.nsc.interactive.Global
  import global._

  def addField(file: AbstractFile, className: String, valName: String, isVar: Boolean, returnTypeOpt: Option[String], target: AddMethodTarget): List[TextChange] =
    addValOrDef(file, className, target, addField(valName, isVar, returnTypeOpt, _))

  private def addField(valName: String, isVar: Boolean, returnTypeOpt: Option[String], classOrObjectDef: Tree): List[TextChange] = {
    val returnStatement = Ident("???")

    val returnType = returnTypeOpt.map(name => TypeTree(newType(name))).getOrElse(new TypeTree)

    val mods = if (isVar) Modifiers(Flag.MUTABLE) withPosition (Tokens.VAR, NoPosition) else NoMods

    val newVal = mkValOrVarDef(mods, valName, returnStatement, returnType)

    val insertField = insertDef(newVal)

    refactor((insertField apply classOrObjectDef).toList)
  }
}