/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import tools.nsc.symtab.Flags
import common.PimpedTrees

trait TreeFactory {

  this: PimpedTrees with common.CompilerAccess =>

  import global._

  object Invisible extends Position

  def mkRenamedSymTree(t: SymTree, name: String): SymTree = (t match {
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
    case t => throw new Exception("Found " + t.getClass.getName)
  }) setPos t.pos

  def mkRenamedTypeTree(t: TypeTree, name: String, originalSymbol: Symbol) = {
    val newType = t.tpe map {
      case TypeRef(pre, `originalSymbol`, args) =>
        new Type {
          override def safeToString: String = name
        }
      case t => t
    }

    val typeTree = t match {
      case att: AppliedTypeTree => att.copy()
      case _ => new TypeTree
    }

    typeTree setType newType
    typeTree setPos t.pos
  }
  
  def mkImportFromStrings(qualifier: String, name: String) = {
    def mapPackageNames(qualifier: String) = {
      qualifier.split("\\.").map(escapeScalaKeywords).mkString(".")
    }
    
    def escapeScalaKeywords(s: String) = {
      if(global.nme.keywords.contains(s.toTermName)) "`"+ s +"`" else s
    }
    
    new Import(Ident(mapPackageNames(qualifier)), new ImportSelector(name, -1, name, -1) :: Nil)
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
    
    val valDef = ValDef(NoMods, name, new TypeTree, rhs)
    def valDefForFunction = ValDef(NoMods, name, new TypeTree, Apply(rhs, Ident(nme.USCOREkw) :: Nil))
    
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

        ValDef(NoMods, valName, new TypeTree(), call)
    }
  }

  def mkDefDef(mods: Modifiers = NoMods, name: String, parameters: List[List[Symbol]] = Nil :: Nil, body: List[Tree]): DefDef = {

    val formalParameters = {
      if (parameters.isEmpty)
        Nil
      else
        parameters map (_ map (s => new ValDef(Modifiers(Flags.PARAM), s.nameString, TypeTree(s.tpe), EmptyTree)))
    }

    DefDef(mods withPosition (Flags.METHOD, NoPosition), name, Nil /*type parameters*/ , formalParameters, TypeTree(body.last.tpe), mkBlock(body))
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
        ValDef(mods | Flags.PARAMACCESSOR, name, tpe, EmptyTree)
    })

    val constructor = {
      val body = List(superArgs) map {
        case Nil =>
          EmptyTree
        case args =>
          Apply(EmptyTree, args)
      }

      DefDef(mods withPosition (Flags.METHOD, NoPosition), nme.CONSTRUCTOR.toString, Nil, constructorArguments, TypeTree(NoType), mkBlock(body))
    }

    ClassDef(
      mods,
      name.toTypeName,
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
  
  class CopyTypeFromOtherTree(t1: Tree) {
    def typeFrom(t2: Tree) = {
      t1.tpe = t2.tpe
      t1
    }
  }

  implicit def typeFromTree(t1: Tree) = new CopyTypeFromOtherTree(t1)
  
  /**
   * Creates a function call `fun` on the selector and passes a function with
   * a single parameter `param` and the body `body`.
   * 
   * Example:
   *  
   *  someExpr becomes someExpr fun (param => body)
   * 
   */
  def mkFunctionCallWithFunctionArgument(selector: Tree, fun: String, param: Name, body: Tree) = {
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
}
