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
    val constructor = (prep.impl.body filter (t => t.hasSymbol && t.symbol.isPrimaryConstructor)).head.asInstanceOf[DefDef]

    // TODO: get rid of this var
    var companionModuleDef: Option[ModuleDef] = None

    def makeApply(className: TermName, constr: DefDef) = {
      val params = constr.vparamss.map(_ map (p => p.symbol))
      val select: Tree = Select(New(Ident(className)), constr.name)
      val constructorCall = constr.vparamss.foldLeft(select)((fun, args) => Apply(fun, args map (p => Ident(p.name))))
      mkDefDef(NoMods, "apply", parameters = params, body = List(constructorCall), typeParameters = prep.tparams) setPos NoPosition
    }

    lazy val companionObject = {
      val impl = Template(Nil, emptyValDef, List(makeApply(prep.name, constructor)))
      val companionObject = ModuleDef(NoMods, prep.name, impl)
      companionModuleDef = Some(companionObject)
      companionObject
    }

    val enclosingDefTree = {
      val owner = prep.symbol.owner
      owner match {
        case s: Symbol if s.isPackageClass => {
          val enclosingPackageWithDeclarationSymbol = owner.ownerChain.dropWhile(s => index.declaration(s.companionModule).isEmpty).head
          index.declaration(enclosingPackageWithDeclarationSymbol.companionModule).get
        }
        case s: Symbol if s.isModuleClass =>
          index.declaration(s.companionModule).get
        case _ => index.declaration(owner).get
      }
    }

    def companionObjectFilter(companionSymbol: Symbol) = filter {
      case m: ModuleDef if (m.hasSymbol) => m.symbol == companionSymbol
    }

    val addConstructorToCompanionObject = transform {
      case m @ ModuleDef(mods, name, t @ Template(parents, self, body)) => {
        companionModuleDef = Some(m)
        val newBody = makeApply(prep.name, constructor) :: body
        ModuleDef(mods, name, Template(parents, self, newBody) setPos NoPosition) replaces m
      }
    }

    val refactorExistingCompanionObject = topdown {
      matchingChildren {
        companionObjectFilter(prep.symbol.companionModule) &> addConstructorToCompanionObject
      }
    }

    val enclosingFilter = filter {
      case `enclosingDefTree` => true
    }

    val isCompanionObjectExisting = predicate {
      (t: Tree) => prep.symbol.companionModule != NoSymbol
    }

    val createCompanionObject = transform {
      case packageDef @ PackageDef(pid, stats) =>
        PackageDef(pid, companionObject :: stats) replaces packageDef
      case classDef @ ClassDef(mods, name, tparams, Template(parents, self, body)) =>
        ClassDef(mods, name, tparams, Template(parents, self, companionObject :: body)) replaces classDef
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, Block(stats, expr)) =>
        DefDef(mods, name, tparams, vparamss, tpt, Block(companionObject :: stats, expr)) replaces defdef
      case valdef @ ValDef(mods, name, tpt, Block(stats, expr)) =>
        ValDef(mods, name, tpt, Block(companionObject :: stats, expr)) replaces valdef
      case moduleDef @ ModuleDef(mods, name, Template(parents, self, body)) =>
        ModuleDef(mods, name, Template(parents, self, companionObject :: body)) replaces moduleDef
    }
    
    // TODO: maybe make that somehow more efficient?
    val insertCompanionObject = topdown {
      enclosingFilter &> createCompanionObject |> id
    }
    val createApplyMethod = isCompanionObjectExisting &> refactorExistingCompanionObject |> insertCompanionObject

    def constructorCallFilter(calls: List[Tree]) = filter {
      case s: Select => calls contains s
    }

    def redirectSingleConstructorCall = transform {
      case s: Select => {
        val newSelect = Select(Ident(companionModuleDef.get.name), "apply")
        newSelect
      }
    }

    def redirectConstructorCalls = {
      val calls = index.occurences(constructor.symbol)
      val callRefactoring = topdown(constructorCallFilter(calls) &> redirectSingleConstructorCall |> id)
      callRefactoring
    }

    val refactoring = createApplyMethod &> redirectConstructorCalls

    Right(transformFile(selection.file, refactoring))
  }

}