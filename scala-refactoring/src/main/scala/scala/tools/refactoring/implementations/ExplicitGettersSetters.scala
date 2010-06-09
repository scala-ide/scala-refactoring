/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

import scala.tools.nsc.util.RangePosition
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.symtab.Types

abstract class ExplicitGettersSetters extends MultiStageRefactoring {
  
  import global._
  
  abstract class PreparationResult {
    def selectedValue: ValDef
  }
  
  class RefactoringParameters
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ValDef] match {
      case Some(valdef) =>
        Right(new PreparationResult {
          val selectedValue = valdef
        })
      case None => Left(new PreparationError("no valdef selected"))
    }
  }
    
  override def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Tree]] = {
    
    import prepared._
    import params._
    
    val template = selection.findSelectedOfType[Template].getOrElse {
      return Left(RefactoringError("no template found"))
    }
    
    def createSetter = selectedValue.symbol.isMutable
    
    val publicName = selectedValue.name.toString.trim
    
    val privateName = "_"+ publicName
    //var flag
    val privateField = selectedValue copy (mods = Modifiers(Flags.PARAMACCESSOR) | Flags.PRIVATE, name = privateName)
    
    val getter = DefDef(
        NoMods withPosition (Flags.METHOD, NoPosition), 
        publicName, 
        Nil, 
        List(Nil), 
        EmptyTree, 
        Block(
            Ident(privateName) :: Nil, EmptyTree))
    
    val setter = DefDef(
        NoMods withPosition (Flags.METHOD, NoPosition), 
        publicName +"_=",
        Nil,
        List(List(ValDef(Modifiers(Flags.PARAM), publicName, TypeTree(selectedValue.tpt.tpe), EmptyTree))), 
        EmptyTree,
        Block(
            Assign(
                Ident(privateName),
                Ident(publicName)) :: Nil, EmptyTree))
    
    val r = transform {
        
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
    
    val changes = ↓(matchingChildren(r)) apply abstractFileToTree(selection.file)
    
    println(changes.get)

    Right(changes toList)
  }
}
