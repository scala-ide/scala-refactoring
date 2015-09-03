package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.common.TextChange

import common.InteractiveScalaCompiler

trait AddValOrDef extends Refactoring with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global
  import global._

  def addValOrDef(file: AbstractFile,  className: String, target: AddMethodTarget, changeFunc: Tree => List[TextChange]): List[TextChange] = {
    val astRoot = abstractFileToTree(file)

    //it would be nice to pass in the symbol and use that rather than compare the name, but it might not be available
    val classOrObjectDef = target match {
      case AddToClosest(offset: Int) => {
        case class UnknownDef(tree: Tree, offset: Int)

        val classAndObjectDefs = astRoot.collect {
          case classDef: ClassDef if classDef.name.decode == className =>
            UnknownDef(classDef, classDef.namePosition.point)
          case moduleDef: ModuleDef if moduleDef.name.decode == className =>
            UnknownDef(moduleDef, moduleDef.namePosition.point)
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

    changeFunc(classOrObjectDef.get)
  }

  protected def insertDef(valOrDef: ValOrDefDef) = {
    def addMethodToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: valOrDef :: Nil) replaces tpl

    transform {
      case implDef: ImplDef => addMethodToTemplate(implDef.impl)
    }
  }

  protected def newType(name: String) = new Type {
    override def safeToString: String = name
  }
}

sealed trait AddMethodTarget
case object AddToClass extends AddMethodTarget
case object AddToObject extends AddMethodTarget
case class AddToClosest(offset: Int) extends AddMethodTarget