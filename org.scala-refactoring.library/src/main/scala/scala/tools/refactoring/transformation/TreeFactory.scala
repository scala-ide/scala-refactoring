/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import tools.nsc.symtab.Flags
import common.PimpedTrees

trait TreeFactory {

  this: PimpedTrees with common.CompilerAccess with TreeTransformations =>

  import global._

  object Invisible extends Position

  def mkRenamedSymTree(t: SymTree, nameString: String): SymTree = {
    val name = newTermName(nameString)
    t match {
      case i: Ident => i.copy(name = name)
      case v: ValDef => v.copy(name = name)
      case d: DefDef => d.copy(name = name)
      case b: Bind => b.copy(name = name)
      case s: Select => s.copy(name = name)
      case c: ClassDef => c.copy(name = name.toTypeName)
      case t: This => t.copy(qual = name.toTypeName)
      case m: ModuleDef => m.copy(name = name)
      case t: TypeDef => t.copy(name = name.toTypeName)
      case t: PackageDef => t.copy(pid = Ident(name) setPos t.pid.pos)
      case t: SelectFromTypeTree => t.copy(name = name.toTypeName)
      case t: NamedArgument => t.copy(nameTree = NameTree(name) replaces t.nameTree)
      case t => throw new Exception("Found " + getSimpleClassName(t))
    }
  } setPos t.pos

  def mkRenamedTypeTree(t: TypeTree, name: String, originalSymbol: Symbol) = {
    val newType = t.tpe map {
      case TypeRef(pre, `originalSymbol`, args) =>
        new Type {
          override def safeToString: String = name
        }
      case t => t
    }

    val typeTree = new TypeTree

    typeTree setType newType
    typeTree setPos t.pos
  }

  def mkImportFromStrings(qualifier: String, name: String) = {
    def mapPackageNames(qualifier: String) = {
      newTermName(qualifier.split("\\.").map(s => escapeScalaKeywordsForImport(newTermName(s))).mkString("."))
    }

    new Import(Ident(mapPackageNames(qualifier)), new ImportSelector(newTermName(name), -1, newTermName(name), -1) :: Nil)
  }

  def mkRenamedImportTree(t: ImportSelectorTree, name: String) =
    ImportSelectorTree(NameTree(name) setPos t.name.pos, t.rename) setPos t.pos

  def mkReturn(s: List[Symbol]): Tree = s match {
    case Nil => EmptyTree
    case x :: Nil => Ident(x) setType x.tpe
    case xs =>
      typer.typed(gen.mkTuple(xs map (s => Ident(s) setType s.tpe))) match {
        case t: Apply => t.fun setPos Invisible; t //don't show the TupleX..
        case t => t
      }
  }

  def mkValDef(name: String, rhs: Tree): ValDef = {

    val valDef = ValDef(NoMods, newTermName(name), new TypeTree, rhs)
    def valDefForFunction = ValDef(NoMods, newTermName(name), new TypeTree, Apply(rhs, Ident(nme.USCOREkw) :: Nil))

    rhs match {
      case rhs: Select if rhs.symbol.isMethod =>
        rhs.symbol.tpe match {
          case _: NullaryMethodType => valDef
          case _ => valDefForFunction
        }
      case _ => valDef
    }
  }

  def mkCallDefDef(name: String, arguments: List[List[Symbol]] = Nil :: Nil, returns: List[Symbol] = Nil): Tree = {

    // currying not yet supported
    val args = arguments.head map (s => Ident(s))

    val call = if (args.isEmpty)
      Select(This(nme.EMPTY.toTypeName) setPos Invisible, name)
    else
      Apply(Select(This(nme.EMPTY.toTypeName) setPos Invisible, name), args)

    returns match {
      case Nil => call
      case returns =>

        // 'val (a, b) =' is represented by various trees, so we cheat and create the assignment in the name of the value:
        val valName = returns match {
          case x :: Nil => x.name.toString
          case xs => "(" + (xs map (_.name) mkString ", ") + ")"
        }

        ValDef(NoMods, newTermName(valName), new TypeTree(), call)
    }
  }

  def mkDefDef(mods: Modifiers = NoMods, name: String, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree], typeParameters: List[TypeDef] = Nil, returnType: Option[TypeTree] = None): DefDef = {
    val formalParameters = {
      if (parameters.isEmpty)
        Nil
      else
        parameters map (_ map (s => new ValDef(Modifiers(Flags.PARAM), newTermName(s.nameString), TypeTree(s.tpe), EmptyTree)))
    }

    DefDef(mods withPosition (Flags.METHOD, NoPosition), newTermName(name), typeParameters, formalParameters, returnType.getOrElse(TypeTree(body.last.tpe)), mkBlock(body))
  }

  def mkApply(mods: Modifiers = NoMods, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree], typeParameters: List[TypeDef] = Nil) = {
    mkDefDef(mods = mods, name = "apply", parameters = parameters, body = body, typeParameters, None)
  }

  def mkHashcode(classSymbol: Symbol, classParamsForHashcode: List[ValDef], callSuper: Boolean, prime: Int = 41) = {
    def mkSingleParamPart(param: ValDef, primeName: TermName, inner: Tree) = {
      val mult = Apply(Select(Ident(primeName), nme.MUL), List(inner))
      Apply(Select(mult, nme.PLUS), List(Select(Ident(param.name), nme.hashCode_)))
    }

    def mkFold(init: Tree, primeName: TermName, paramsForHashcode: List[ValDef]) = {
      paramsForHashcode.foldLeft(init)((inner, param) => {
        mkSingleParamPart(param, primeName, inner)
      })
    }

    val primeVal = mkValDef("prime", Literal(Constant(prime)))
    val oneLiteral = Literal(Constant(1))
    val (startFactor, remainingParams): (Tree, List[ValDef]) = if(callSuper) {
      (Apply(Select(Super(classSymbol, newTypeName("")), nme.hashCode_), Nil), classParamsForHashcode)
    } else {
      classParamsForHashcode match {
        case Nil => (Ident(primeVal.name), Nil)
        case p::ps =>
          (Apply(Select(Ident(primeVal.name), nme.PLUS), List(Select(Ident(p.name), nme.hashCode_))), ps)
      }
    }
    val computation = mkFold(startFactor, primeVal.name, remainingParams)
    mkDefDef((NoMods withPosition(Flags.OVERRIDE, NoPosition)) | Flags.OVERRIDE, "hashCode", body = List(primeVal, computation))
  }

  def mkCanEqual(classSymbol: Symbol) = {
    val paramStr = newTermName("other")
    val otherParamSymbol = NoSymbol.newValue(paramStr)
    otherParamSymbol.setInfo(classSymbol.ancestors.last.tpe)
    val instanceCheck = TypeApply(Select(Ident(paramStr), nme.isInstanceOf_), List(TypeTree(classSymbol.tpe)))
    mkDefDef(NoMods, "canEqual", parameters = List(List(otherParamSymbol)), body = List(instanceCheck))
  }

  def mkEquals(classSymbol: Symbol, classParamsForEqual: List[ValDef], callSuper: Boolean) = {
    val paramStr = newTermName("other")
    val otherParamSymbol = NoSymbol.newValue(paramStr)
    otherParamSymbol.setInfo(classSymbol.ancestors.last.tpe)

    val canEqual = Apply(Select(Ident("that"), nme.canEqual_), List(This(classSymbol)))
    val superCall = Apply(Select(Super(classSymbol, newTypeName("")), nme.equals_), List(Ident("that")))
    val startCall = if (callSuper) {
      Apply(Select(superCall, nme.ZAND), List(canEqual))
    } else {
      canEqual
    }
    val body = {
      classParamsForEqual.foldLeft(startCall)((apply, param) => {
        val singleParamEqual = Apply(Select(Ident(param.name), nme.EQ), List(Select(Ident("that"), param.name)))
        Apply(Select(apply, nme.ZAND), List(singleParamEqual))
      })
    }
    val bind = Bind(newTermName("that"), Typed(Ident(""), TypeTree(classSymbol.tpe)))
    val matchingTypesCase = CaseDef(bind, EmptyTree, body)
    val defaultCase = CaseDef(Ident(nme.USCOREkw), EmptyTree, Literal(Constant(false)))
    val matchTree = Match(Ident(paramStr), List(matchingTypesCase, defaultCase))
    mkDefDef((NoMods withPosition (Flags.OVERRIDE, NoPosition)) | Flags.OVERRIDE, "equals", parameters = List(List(otherParamSymbol)), body = List(matchTree))
  }

  def mkBlock(trees: List[Tree]): Block = trees match {
    case Nil => throw new Exception("can't make block from 0 trees")
    case x :: Nil => Block(x :: Nil, EmptyTree)
    case xs => Block(xs.init, xs.last)
  }

  def mkClass(
    mods: Modifiers = NoMods,
    name: String,
    tparams: List[TypeDef] = Nil,
    argss: List[List[(Modifiers, String, Tree)]] = Nil,
    body: List[Tree] = Nil,
    parents: List[Tree] = Nil,
    superArgs: List[Tree] = Nil) = {

    val constructorArguments = argss map (_ map {
      case (mods, name, tpe) =>
        ValDef(mods | Flags.PARAMACCESSOR, newTermName(name), tpe, EmptyTree)
    })

    val constructor = {
      val body = List(superArgs) map {
        case Nil =>
          EmptyTree
        case args =>
          Apply(EmptyTree, args)
      }

      DefDef(mods withPosition (Flags.METHOD, NoPosition), nme.CONSTRUCTOR, Nil, constructorArguments, TypeTree(NoType), mkBlock(body))
    }

    ClassDef(
      mods,
      newTypeName(name),
      tparams,
      Template(
        parents,
        emptyValDef,
        constructor :: constructorArguments.flatten ::: body))
  }

  def mkCaseClass(
    mods: Modifiers = NoMods,
    name: String,
    tparams: List[TypeDef] = Nil,
    argss: List[List[(Modifiers, String, Tree)]] = Nil,
    body: List[Tree] = Nil,
    parents: List[Tree] = Nil,
    superArgs: List[Tree] = Nil) = {

    mkClass(mods withPosition (Flags.CASE, NoPosition), name, tparams, argss, body, parents, superArgs)
  }

  implicit class CopyTypeFromOtherTree(t1: Tree) {
    def typeFrom(t2: Tree) = {
      t1.setType(t2.tpe)
    }
  }

  /**
   * Creates a function call `fun` on the selector and passes a function with
   * a single parameter `param` and the body `body`.
   *
   * Example:
   *
   *  someExpr becomes someExpr fun (param => body)
   *
   */
  def mkFunctionCallWithFunctionArgument(selector: Tree, fun: String, param: TermName, body: Tree) = {
    Apply(
      Select(selector, newTermName(fun)),
      List(Function(List(ValDef(Modifiers(Flags.PARAM), param, EmptyTree, EmptyTree)), body))
    ) typeFrom body
  }

  /**
   * Creates a function call `fun` on the selector and passes a function with
   * no parameter and the body `body`.
   *
   * Example:
   *
   *  someExpr becomes someExpr fun (body)
   */
  def mkFunctionCallWithZeroArgFunctionArgument(selector: Tree, fun: String, body: Tree) = {
    Apply(
      Select(selector, newTermName(fun)),
      List(Function(Nil, body))
    ) typeFrom body
  }

  def mkImportTrees(trees: List[Select], enclosingPackage: String) = {

    def importsFromSamePackage(t: Tree) = {
      asSelectorString(t) == enclosingPackage
    }

    trees flatMap {
      // warning if binding is never used! and quickfix to replace with `_`!
      case Select(selector, _) if importsFromSamePackage(selector) =>
        None
      case select @ Select(expr, name) =>

        // we don't want to see imports like "java.this.lang..."
        val removeThisTrees = {
          matchingChildren {
            transform {
              case t: This =>
                // expand to the full package name
                val parents = ancestorSymbols(t)
                Ident(parents map (_.nameString) mkString ".")
            }
          }
        }

        // copy the tree and delete all positions so the full path will be written
        val newExpr = topdown(setNoPosition &> removeThisTrees) apply expr.duplicate getOrElse expr

        val typeName = select.symbol match {
          case NoSymbol => name.toChars.mkString
          case symbol => symbol.nameString
        }

        Some(Import(newExpr, List(new ImportSelector(if(typeName == name.toString) name else newTypeName(typeName), -1, name, -1))))
    }
  }
}
