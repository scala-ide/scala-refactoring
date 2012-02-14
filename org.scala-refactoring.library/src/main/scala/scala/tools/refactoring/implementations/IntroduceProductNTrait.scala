package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.transformation.TreeFactory

abstract class IntroduceProductNTrait extends MultiStageRefactoring with common.InteractiveScalaCompiler with analysis.Indexes with TreeFactory with PimpedTrees {

  import global._
  
  case class PreparationResult(classDef: ClassDef, classParams: List[(String, Boolean)])
  
  case class RefactoringParameters(callSuper: Boolean = true, paramsFilter: Option[String => Boolean] = None)
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classdef) => {
        Right(PreparationResult(classdef, classdef.impl.nonPrivateClassParameters.map(t => (t._1.nameString, t._2))))
      }
    }
  }
  
  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val nonPrivateParams = prep.classDef.impl.nonPrivateClassParameters

    // if no params filter is supplied in the refactoring parameters we use only immutable class parameters
    val paramsFilter = {
      // collect all immutable class parameters 
      lazy val immutableParams = nonPrivateParams collect { case t if !t._2 => t._1.nameString }
      params.paramsFilter getOrElse ((str: String) => immutableParams contains str)
    }
    
    val paramsForProduct = nonPrivateParams collect { case t if paramsFilter(t._1.nameString)=> t._1 }
    
    val arity = paramsForProduct.length
    val paramsTypenames = paramsForProduct.map(v => v.tpt.nameString)
    val productParentName = newTermName("Product" + arity + "[" + paramsTypenames.mkString(", ") + "]")
    
    
    def makeElemProjection(elem: ValDef, pos: Int) = {
      val body = List(Ident(elem.name))
      mkDefDef(name = "_"+pos, body = body)
    }
    
    val projections = paramsForProduct.zipWithIndex.map(t => makeElemProjection(t._1, t._2 + 1))
        
    
    val classSymbol = prep.classDef.symbol
    
    val canEqual = mkCanEqual(classSymbol)
    val hashcode = mkHashcode(classSymbol, paramsForProduct, params.callSuper, 41)
    val equals = mkEquals(classSymbol, paramsForProduct, params.callSuper)
    
    val equalityMethods = canEqual::equals::hashcode::Nil
    
    val classFilter = filter {
      case prep.classDef.impl => true
    }
    
    def addProductTrait() = transform {
      case Template(parents, self, body) => Template(parents:::List(Ident(productParentName)), self, projections:::equalityMethods:::body)
    }
    
    val refactoring = topdown {
      matchingChildren {
        classFilter &> addProductTrait
      }
    }
    
    Right(transformFile(selection.file, refactoring))
  }
}