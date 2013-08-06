/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.symtab.Flags

abstract class ExplicitGettersSetters extends MultiStageRefactoring with common.InteractiveScalaCompiler {

  import global._

  type PreparationResult = ValDef

  class RefactoringParameters

  def prepare(s: Selection) = {
    s.findSelectedOfType[ValDef] match {
      case Some(valdef) => Right(valdef)
      case None => Left(new PreparationError("no valdef selected"))
    }
  }

  override def perform(selection: Selection, selectedValue: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {

    val template = selection.findSelectedOfType[Template].getOrElse {
      return Left(RefactoringError("no template found"))
    }

    val createSetter = selectedValue.symbol.isMutable

    val publicName = selectedValue.name.toString.trim

    val privateName = "_"+ publicName

    val privateFieldMods = if(createSetter)
      Modifiers(Flags.PARAMACCESSOR).
        withPosition (Flags.PRIVATE, NoPosition).
        withPosition (Tokens.VAR, NoPosition)
    else
      Modifiers(Flags.PARAMACCESSOR)

    val privateField = selectedValue copy (mods = privateFieldMods, name = newTermName(privateName))

    val getter = DefDef(
        mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition),
        name = newTermName(publicName),
        tparams = Nil,
        vparamss = Nil,
        tpt = EmptyTree,
        rhs = Block(
            Ident(privateName) :: Nil, EmptyTree))

    val setter = DefDef(
        mods = Modifiers(Flags.METHOD) withPosition (Flags.METHOD, NoPosition),
        name = newTermName(publicName +"_="),
        tparams = Nil,
        vparamss = List(List(ValDef(Modifiers(Flags.PARAM), newTermName(publicName), TypeTree(selectedValue.tpt.tpe), EmptyTree))),
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

    Right(transformFile(selection.file, topdown(matchingChildren(insertGetterSettersTransformation))))
  }
}
