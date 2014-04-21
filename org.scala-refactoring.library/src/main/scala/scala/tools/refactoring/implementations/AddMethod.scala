package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.common.TextChange

abstract class AddMethod extends AddValOrDef {

  val global: tools.nsc.interactive.Global
  import global._

  def addMethod(file: AbstractFile, className: String, methodName: String, parameters: List[List[(String, String)]], typeParameters: List[String], returnType: Option[String], target: AddMethodTarget): List[TextChange] =
    addValOrDef(file, className, target, addMethod(methodName, parameters, typeParameters, returnType, _))

  def addMethod(file: AbstractFile, className: String, methodName: String, parameters: List[List[(String, String)]], returnType: Option[String], target: AddMethodTarget): List[TextChange] =
    addMethod(file, className, methodName, parameters, Nil, returnType, target)

  private def addMethod(methodName: String, parameters: List[List[(String, String)]], typeParameters: List[String], returnTypeOpt: Option[String], classOrObjectDef: Tree): List[TextChange] = {
    val nscParameters = for (paramList <- parameters) yield for ((paramName, typeName) <- paramList) yield {
      val paramSymbol = NoSymbol.newValue(newTermName(paramName))
      paramSymbol.setInfo(newType(typeName))
      paramSymbol
    }

    val typeParams = if (typeParameters.nonEmpty) {
      val typeDef = new TypeDef(NoMods, newTypeName(typeParameters.mkString(", ")), Nil, EmptyTree)
      List(typeDef)
    } else Nil

    val returnStatement = Ident("???") :: Nil
    val newDef = mkDefDef(NoMods, methodName, nscParameters, returnStatement, typeParams, returnTypeOpt = returnTypeOpt.map(name => TypeTree(newType(name))))

    def addMethodToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: newDef :: Nil) replaces tpl

    val insertMethodCall = transform {
      case ClassDef(_, _, _, tpl) => addMethodToTemplate(tpl)
      case ModuleDef(_, _, tpl) => addMethodToTemplate(tpl)
    }

    refactor((insertMethodCall apply classOrObjectDef).toList)
  }
}