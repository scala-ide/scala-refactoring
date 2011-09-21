package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change
import transformation.TreeFactory

abstract class MoveConstructorToCompanionObject extends MultiStageRefactoring with common.InteractiveScalaCompiler with common.PimpedTrees with analysis.Indexes with TreeFactory {

  import global._
  
  type PreparationResult = ClassDef
  class RefactoringParameters 
  
  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classdef) => Right(classdef)
    }
  }
 
  override def perform(selection: Selection, prep: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Change]] = {
    val constructor = (prep.impl.body filter (_.symbol.isConstructor)).head.asInstanceOf[DefDef]
    
    def companionObjectFilter(companionSymbol: Symbol) = filter {
      case m: ModuleDef if(m.hasSymbol)=> m.symbol == companionSymbol
    }
    
    def makeApply() = {
      // TODO: make this work for constructors with multiple argument lists?
      val params = constructor.vparamss.map(_ map (p => p.symbol))
      val select = Select(New(Ident(prep.name)), constructor.name)
      val constructorCall = Apply(select, constructor.vparamss(0).map(p => Ident(p.name)))
      mkDefDef(NoMods, "apply", parameters = params, body = List(constructorCall)) setPos NoPosition
    }
    
    val addConstructorToCompanionObject = transform {
      case m @ ModuleDef(mods, name, t @ Template(parents, self, body)) => {        
        val newBody = makeApply::body
        ModuleDef(mods, name, Template(parents, self, newBody) setPos NoPosition) replaces m
      }
    }
    
    val refactorCompanionObject = topdown {
      matchingChildren {
        companionObjectFilter(prep.symbol.companionModule) &> addConstructorToCompanionObject
      }
    }

    // TODO: enclosing Tree doesn't have to be a package, implement other cases
    val enclosingPackage = index.declaration(prep.symbol.enclosingPackage).get
    
    val isCompanionObjectExisting = predicate { 
      (t: Tree) => t match {
        case enclosingPackage: PackageDef => enclosingPackage.children exists (_.symbol == prep.symbol.companionModule)
      }
    }
    
    val createCompanionObject = transform {
      case packageDef @ PackageDef(pid, stats) => {
        val impl = Template(Nil, emptyValDef, List(makeApply))
        val companionObject = ModuleDef(NoMods, prep.name, impl)
        PackageDef(pid, companionObject::stats) replaces packageDef
      }
    }
    
    val trees = List((isCompanionObjectExisting &> refactorCompanionObject |> createCompanionObject) (enclosingPackage)) flatMap (t => t)
    
    Right(refactor(trees)) 
  }
  
}