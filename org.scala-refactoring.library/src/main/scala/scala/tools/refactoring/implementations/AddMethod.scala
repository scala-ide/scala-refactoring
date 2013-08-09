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
    val classOrObjectDef = target match {
      case AddToClosest(offset: Int) => {
        case class UnknownDef(tree: Tree, offset: Int)

        val classAndObjectDefs = astRoot.collect {
          case classDef: ClassDef if classDef.name.decode == className =>
            UnknownDef(classDef, classDef.namePosition.offset.getOrElse(0))
          case moduleDef: ModuleDef if moduleDef.name.decode == className =>
            UnknownDef(moduleDef, moduleDef.namePosition.offset.getOrElse(0))
        }

        //the class/object definition just before the given offset
        classAndObjectDefs.sortBy(_.offset).reverse.find(_.offset < offset).map(_.tree)
      }
      case _ => {
        astRoot.find {
          case classDef: ClassDef if target == AddToClass => classDef.name.decode == className
          case moduleDef: ModuleDef if target == AddToObject => moduleDef.name.decode == className
          case _ => false
        }
      }
    }

    addMethod(file, className, methodName, parameters, returnType, classOrObjectDef.get)
  }

  private def addMethod(file: AbstractFile, className: String, methodName: String, parameters: List[List[(String, String)]], returnTypeOpt: Option[String], classOrObjectDef: Tree): List[TextChange] = {
    val nscParameters = for (paramList <- parameters) yield for ((paramName, typeName) <- paramList) yield {
      val paramSymbol = NoSymbol.newValue(newTermName(paramName))
      paramSymbol.setInfo(newType(typeName))
      paramSymbol
    }

    val returnStatement = Ident("???") :: Nil
    val newDef = mkDefDef(NoMods, methodName, nscParameters, returnStatement, returnTypeOpt = returnTypeOpt.map(name => TypeTree(newType(name))))

    def addMethodToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: newDef :: Nil) replaces tpl

    val insertMethodCall = transform {
      case ClassDef(_, _, _, tpl) => addMethodToTemplate(tpl)
      case ModuleDef(_, _, tpl) => addMethodToTemplate(tpl)
    }

    refactor((insertMethodCall apply classOrObjectDef).toList)
  }

  private def newType(name: String) = new Type {
    override def safeToString: String = name
  }
}

sealed trait AddMethodTarget
case object AddToClass extends AddMethodTarget
case object AddToObject extends AddMethodTarget
case class AddToClosest(offset: Int) extends AddMethodTarget
