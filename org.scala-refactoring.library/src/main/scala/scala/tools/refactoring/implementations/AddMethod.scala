package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.common.TextChange

import common.InteractiveScalaCompiler

abstract class AddMethod extends Refactoring with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global
  import global._

  def addMethod(file: AbstractFile, className: String, methodName: String, parameters: List[List[(String, String)]], returnType: Option[String], target: AddMethodTarget): List[TextChange] = {
    val astRoot = abstractFileToTree(file)

    //it would be nice to pass in the symbol and use that rather than compare the name, but it might not be available
    val classOrObjectDef = astRoot.find {
      case classDef: ClassDef if target == AddToClass => classDef.name.decode == className
      case moduleDef: ModuleDef if target == AddToObject => moduleDef.name.decode == className
      case _ => false
    }

    val nscParameters = for (paramList <- parameters) yield for ((paramName, typeName) <- paramList) yield {
      val paramSymbol = NoSymbol.newValue(newTermName(paramName))
      paramSymbol.setInfo(newType(typeName))
      paramSymbol
    }

    val returnStatement = Ident("???") :: Nil
    val newDef = mkDefDef(NoMods, methodName, nscParameters, returnStatement, returnType = returnType.map(name => TypeTree(newType(name))))

    def addMethodToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: newDef :: Nil) replaces tpl

    val insertMethodCall = transform {
      case ClassDef(_, _, _, tpl) => addMethodToTemplate(tpl)
      case ModuleDef(_, _, tpl) => addMethodToTemplate(tpl)
    }

    refactor((insertMethodCall apply classOrObjectDef.get).toList)
  }

  private def newType(name: String) = new Type {
    override def safeToString: String = name
  }
}

sealed trait AddMethodTarget
case object AddToClass extends AddMethodTarget
case object AddToObject extends AddMethodTarget
