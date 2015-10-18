package scala.tools.refactoring
package implementations

import scala.tools.refactoring.common.Change

/**
 * Generates an apply-method in the companion object that matches the primary
 * constructor of the selected class. All calls to the primary constructor
 * are redirected the the newly generated apply-method.
 */
abstract class MoveConstructorToCompanionObject extends MultiStageRefactoring with ParameterlessRefactoring with common.InteractiveScalaCompiler with analysis.Indexes {

  import global._

  type PreparationResult = ClassDef

  def prepare(s: Selection) = {
    s.findSelectedOfType[ClassDef] match {
      case None => Left(PreparationError("no class def selected"))
      case Some(classdef) => Right(classdef)
    }
  }

  override def perform(selection: Selection, prep: PreparationResult): Either[RefactoringError, List[Change]] = {
    val constructors = prep.impl.body.collect{case d: DefDef if d.symbol.isConstructor => d}
    val primaryConstructor = prep.impl.primaryConstructor.headOption
    val constructor = primaryConstructor.getOrElse(constructors.head)

    def makeApply(className: TermName, constr: DefDef) = {
      val params = constr.vparamss.map(_ map (p => p.symbol))
      val select: Tree = Select(New(Ident(className)), constr.name)
      val constructorCall = constr.vparamss.foldLeft(select)((fun, args) => Apply(fun, args map (p => Ident(p.name))))
      mkApply(parameters = params, body = List(constructorCall), typeParameters = prep.tparams) setPos NoPosition
    }

    def companionObjectFilter(companionSymbol: Symbol) = filter {
      case m: ModuleDef if (m.hasSymbol) => m.symbol == companionSymbol
    }

    val addConstructorToCompanionObject = transform {
      case m @ ModuleDef(mods, name, t @ Template(parents, self, body)) => {
        val params = constructor.vparamss.map(_ map (p => p.symbol))
        val select: Tree = Select(New(Ident(name)), constructor.name)
        val constructorCall = constructor.vparamss.foldLeft(select)((fun, args) => Apply(fun, args map (p => Ident(p.name))))
        val newBody = makeApply(prep.name.toTermName, constructor) :: body
        ModuleDef(mods, name, Template(parents, self, newBody) replaces t) replaces m
      }
    }

    val refactorExistingCompanionObject = topdown {
      matchingChildren {
        companionObjectFilter(prep.symbol.companionModule) &> addConstructorToCompanionObject
      }
    }

    lazy val companionObject = {
      val impl = Template(Nil, emptyValDef, List(makeApply(prep.name.toTermName, constructor)))
      val companionObject = ModuleDef(NoMods, prep.name.toTermName, impl)
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

    val createCompanionObject = transform {
      case packageDef @ PackageDef(pid, stats) =>
        PackageDef(pid, companionObject :: stats) replaces packageDef
      case classDef @ ClassDef(mods, name, tparams, t @ Template(parents, self, body)) =>
        ClassDef(mods, name, tparams, Template(parents, self, companionObject :: body) replaces t) replaces classDef
      case defdef @ DefDef(mods, name, tparams, vparamss, tpt, b @ Block(stats, expr)) =>
        DefDef(mods, name, tparams, vparamss, tpt, Block(companionObject :: stats, expr) replaces b) replaces defdef
      case valdef @ ValDef(mods, name, tpt, b @ Block(stats, expr)) =>
        ValDef(mods, name, tpt, Block(companionObject :: stats, expr) replaces b) replaces valdef
      case moduleDef @ ModuleDef(mods, name, t @ Template(parents, self, body)) =>
        ModuleDef(mods, name, Template(parents, self, companionObject :: body) replaces t) replaces moduleDef
    }

    val enclosingFilter = filter {
      case tree: Tree if tree.hasSymbol && tree.symbol == enclosingDefTree.symbol => true
    }

    val insertCompanionObject = topdown {
      enclosingFilter &> createCompanionObject |> id
    }

    val isCompanionObjectExisting = predicate {
      (t: Tree) => prep.symbol.companionModule != NoSymbol
    }

    val createApplyMethod = isCompanionObjectExisting &> refactorExistingCompanionObject |> insertCompanionObject

    def constructorCallFilter(calls: List[Tree]) = filter {
      case Apply(s @ Select(_, _), _) => calls contains s
    }

    def redirectSingleConstructorCall = transform {
      case a: Apply=> {
        val applySelect = Select(Ident(prep.name), nme.apply)
        Apply(applySelect, a.args) replaces a
      }
    }

    val redirectConstructorCalls = {
      val calls = index.occurences(constructor.symbol)
      val callRefactoring = topdown(constructorCallFilter(calls) &> redirectSingleConstructorCall |> id)
      callRefactoring
    }

    val sourcefileChanges = transformFile(selection.file, createApplyMethod)

    val occurrences = index.occurences(constructor.symbol)
    val constructorCallsChanges: List[Change] = refactor(index.rootsOf(occurrences) flatMap (redirectConstructorCalls(_)))

    val changes = sourcefileChanges:::constructorCallsChanges
    Right(changes)
  }

}
