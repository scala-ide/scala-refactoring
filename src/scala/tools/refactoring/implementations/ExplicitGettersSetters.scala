package scala.tools.refactoring.implementations

import scala.tools.nsc.util.RangePosition
import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.symtab.Types

abstract class ExplicitGettersSetters(override val global: Global) extends MultiStageRefactoring(global) {
  
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
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, TreeModifications] = {
    
    import prepared._
    import params._
    
    val template = selection.findSelectedOfType[Template].getOrElse {
      return Left(RefactoringError("no template found"))
    }
    
    def createSetter = selectedValue.symbol.isMutable
    
    val publicName = selectedValue.name.toString.trim
    
    val privateName = "_"+ publicName
    
    val privateField = selectedValue copy (name = privateName)
    
    val getter = DefDef(
        Modifiers(Flags.METHOD), 
        publicName, 
        Nil, 
        List(Nil), 
        EmptyTree, 
        Block(
            Ident(privateName) :: Nil, EmptyTree))
    
    val setter = DefDef(
        Modifiers(Flags.METHOD), 
        publicName +"_=", 
        Nil,
        List(List(ValDef(NoMods, publicName, TypeTree(selectedValue.tpt.tpe), EmptyTree))), 
        EmptyTree, 
        Block(
            Assign(
                Ident(privateName),
                ValDef(NoMods, publicName, TypeTree(NoType), EmptyTree)) :: Nil, EmptyTree))
    
    val changes = new ModificationCollector {
      transform(selection.file) {
        
        case tpl: Template if tpl == template =>
        
          val classParameters = tpl.body.map {
            case t: ValDef if t == selectedValue => privateField setPos t.pos
            case t => t 
          }
          
          val accessors = if(createSetter) 
            getter :: setter :: Nil
          else
            getter :: Nil
          
          tpl.copy(body = accessors ::: classParameters) setPos tpl.pos
      }
    }

    Right(changes)
  }
}
