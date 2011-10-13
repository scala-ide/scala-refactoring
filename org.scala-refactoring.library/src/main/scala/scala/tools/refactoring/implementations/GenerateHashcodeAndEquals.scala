package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.transformation.TreeFactory
import scala.reflect.generic.ModifierFlags
import scala.tools.refactoring.common.PimpedTrees

abstract class GenerateHashcodeAndEquals extends MultiStageRefactoring with common.InteractiveScalaCompiler with analysis.Indexes with TreeFactory with PimpedTrees {

  import global._
  
  type PreparationResult = ClassDef
  
  /** A function that takes a class parameter name and decides
   *  whether this parameter should be used in equals/hashCode
   *  computations, and a boolean that indicates whether calls
   *  to super should be used or not.
   */
  type RefactoringParameters = (Option[String => Boolean], Boolean) // TODO: change this to ValDef => Boolean and find a way to make tests work
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classdef) => {
        val equalsOption = classdef.impl.body collectFirst {
          case d @ DefDef(_, name, _, _, _, _) if(name.toString.trim == "equals" || name.toString.trim == "hashCode") => d
        }
        equalsOption match {
          case Some(_) => Left(PreparationError("equals or hashCode already existing"))
          case None => Right(classdef)
        }
      }
    }
  }
  
  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val body = prep.impl.body
    
    lazy val immutableParams = body collect {
      case v @ ValDef(mods, name, _, _) if mods.isParamAccessor && !mods.isMutable => name.toString.trim
    }
    
    val paramsFilter = params._1 getOrElse ((str: String) => immutableParams contains str)
    val primaryConstructor = body.flatMap { tree =>
      tree match {
        case defdef: DefDef if defdef.symbol.isPrimaryConstructor => Some(defdef)
        case _ => None
      }
    }.head
    
    val nonPrivateParams = body collect {
      case d @ DefDef(mods, name, _, _, _, _) if mods.isParamAccessor => name.toString.trim
    }
    
    val paramsForEqual = primaryConstructor.vparamss.flatten.filter {
      valdef => (nonPrivateParams contains valdef.name.toString.trim) && paramsFilter(valdef.name.toString.trim)
    }
    
    val paramStr = "other"
    
    val otherParamSymbol = NoSymbol.newValue(paramStr)
    otherParamSymbol.setInfo(prep.symbol.ancestors.last.tpe)
    
    val canEqual = {
      val instanceCheck = TypeApply(Select(Ident(paramStr), "isInstanceOf"), List(TypeTree(prep.symbol.tpe)))
      mkDefDef(NoMods, "canEqual", parameters = List(List(otherParamSymbol)), body = List(instanceCheck))
    }
    
    val hashcode = {
      val select = Select(Literal(41), "+")
      val apply = Apply(select, List(Select(Ident(paramsForEqual.head.name), "hashCode")))
      val startFactor: Tree = if(params._2) {
        Apply(Select(Super(prep.symbol, "".toTypeName), "hashCode"), Nil)
      } else {
        Literal(1)
      }
      val body = paramsForEqual.foldLeft(startFactor)((inner, param) => {
        val mult = Apply(Select(Literal(41), "*"), List(inner))
        Apply(Select(mult, "+"), List(Select(Ident(param.name), "hashCode")))
      })
      mkDefDef(Modifiers(ModifierFlags.OVERRIDE), "hashCode", body = List(body))
    }
    
    val equals = {
      val canEqual = Apply(Select(Ident("that"), "canEqual"), List(This(prep.symbol)))
      val superCall = Apply(Select(Super(prep.symbol, "".toTypeName), "equals"), List(Ident("that")))
      val startCall = if(params._2) {
        Apply(Select(superCall, "&&"), List(canEqual))
      } else {
        canEqual
      }
      val body = {
        paramsForEqual.foldLeft(startCall)((apply, param) => {
          val singleParamEqual = Apply(Select(Ident(param.name), "=="), List(Select(Ident("that"), param.name)))
          Apply(Select(apply, "&&"), List(singleParamEqual))
        })
      }
      val bind = Bind("that", Typed(Ident(""), TypeTree(prep.symbol.tpe)))
      val matchingTypesCase = CaseDef(bind, EmptyTree, body)
      val defaultCase = CaseDef(Ident("_"), EmptyTree, Literal(false))
      val matchTree = Match(Ident(paramStr), List(matchingTypesCase, defaultCase))
      mkDefDef(Modifiers(ModifierFlags.OVERRIDE), "equals", parameters = List(List(otherParamSymbol)), body=List(matchTree))
    }
    
    val impl = Template(prep.impl.parents, prep.impl.self, canEqual::equals::hashcode::prep.impl.body) replaces prep.impl
    
    Right(refactor(List(impl)))
  }
  
}