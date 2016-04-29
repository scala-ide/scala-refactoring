package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports.Dependencies

class OrganizeImportsWithMacrosTest extends OrganizeImportsBaseTest {
  private def organizeWithTypicalParams(pro: FileSet) = organizeCustomized()(pro)

  private def organizeCustomized(
      groupPkgs: List[String] = List("java", "scala", "org", "com"),
      useWildcards: Set[String] = Set("scalaz", "scalaz.Scalaz"),
      dependencies: Dependencies.Value = Dependencies.RemoveUnneeded,
      organizeLocalImports: Boolean = true)(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = {
      val groupImports = refactoring.GroupImports(groupPkgs)
      val alwaysUseWildcards = refactoring.AlwaysUseWildcards(useWildcards)

      new refactoring.RefactoringParameters(
          options =
            refactoring.ExpandImports ::
            refactoring.PrependScalaPackage ::
            alwaysUseWildcards ::
            refactoring.SortImports ::
            groupImports ::
            Nil,
          deps = dependencies,
          organizeLocalImports = organizeLocalImports)
    }
  }.mkChanges

  @Ignore("""Needs CLASSPATH set to project classpath to allow interactive global to see macro defined below.
      Go to 'Environment' tab of launch configuration and add CLASSPATH variable set to {project_classpath:org.scala-refactoring.library}.""")
  @Test
  def shouldOrganizeImports() = new FileSet {
    """
    /*<-*/
    package acme

    import scala.tools.refactoring.tests.implementations.imports.OrganizeImportsWithMacros.FakeType
    import scala.tools.refactoring.tests.implementations.imports.OrganizeImportsWithMacros.Macro

    case class SomeCaseClass(x: Int)

    object MacroConsumer {
      import scala.tools.refactoring.tests.implementations.imports.OrganizeImportsWithMacros.Macro._

      implicit val someClassCallMe = Macro.callMe[SomeCaseClass]
    }

    case class Bug2(x: SomeCaseClass)

    object Bug2 {
      import MacroConsumer._

      implicit val bug2CallMe = Macro.callMe[Bug2]
    }
    """ isNotModified
  } applyRefactoring organizeWithTypicalParams
}

object OrganizeImportsWithMacros {
  import scala.language.experimental.macros
  import scala.language.higherKinds
  import scala.reflect.macros.blackbox.Context

  trait FakeType[A]

  object Macro {
    implicit val fakeTypeInt = new FakeType[Int] {}
    def callMe[A] = macro MacroTest.macroImpl[A, FakeType]
  }

  /**
   * Implementation for the macro inspired by Play's `JsMacroImpl.formatImpl` to test if given import is preserved
   * when macro uses its content (e.g. types delivered by import) in code modification.
   */
  object MacroTest {

    def macroImpl[A, N[_]](c: Context)(implicit atag: c.WeakTypeTag[A], natag: c.WeakTypeTag[N[A]]): c.Expr[FakeType[A]] = {

      import c.universe._

      val companioned = weakTypeOf[A].typeSymbol
      val companionObject = companioned.companion
      val companionType = companionObject.typeSignature

      // First find the unapply for the object
      val unapply = companionType.decl(TermName("unapply"))

      val effectiveUnapply = Seq(unapply).find(_ != NoSymbol) match {
        case None => c.abort(c.enclosingPosition, "No unapply function found")
        case Some(s) => s.asMethod
      }

      val unapplyReturnTypes: Option[List[Type]] = effectiveUnapply.returnType match {
        case TypeRef(_, _, Nil) =>
          c.abort(c.enclosingPosition, s"Unapply of $companionObject has no parameters. Are you using an empty case class?")

        case TypeRef(_, _, args) =>
          args.head match {
            case t @ TypeRef(_, _, Nil) => Some(List(t))
            case t @ TypeRef(_, _, args) =>
              import c.universe.definitions.TupleClass
              if (!TupleClass.seq.exists(tupleSym => t.baseType(tupleSym) ne NoType)) Some(List(t))
              else if (t <:< typeOf[Product]) Some(args)
              else None
            case _ => None
          }
        case _ => None
      }

      // Now the apply methods for the object
      val applies =
        companionType.decl(TermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s => s.asTerm.alternatives
        }

      // Find an apply method that matches the unapply
      val maybeApply = applies.collectFirst {
        case (apply: MethodSymbol) if apply.paramLists.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes => apply
      }

      val params = maybeApply match {
        case Some(apply) => apply.paramLists.head //verify there is a single parameter group
        case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
      }

      // Now we find all the implicits that we need
      final case class Implicit(paramName: Name, paramType: Type, neededImplicit: Tree, isRecursive: Boolean, tpe: Type)

      val createImplicit = { (name: Name, implType: c.universe.type#Type) =>
        val (isRecursive, tpe) = implType match {
          case TypeRef(_, t, args) =>
            val isRec = args.exists(_.typeSymbol == companioned)
            val tp = implType
            (isRec, tp)
          case TypeRef(_, t, _) =>
            (false, implType)
        }

        // builds N implicit from expected type
        val neededImplicitType = appliedType(natag.tpe.typeConstructor, tpe :: Nil)
        // infers implicit
        val neededImplicit = c.inferImplicitValue(neededImplicitType)
        Implicit(name, implType, neededImplicit, isRecursive, tpe)
      }

      val effectiveInferredImplicits = params.map { param => createImplicit(param.name, param.typeSignature) }

      // if any implicit is missing, abort
      val missingImplicits = effectiveInferredImplicits.collect { case Implicit(_, t, impl, rec, _) if impl == EmptyTree && !rec => t }
      if (missingImplicits.nonEmpty)
        c.abort(c.enclosingPosition, s"No implicit fakeType for ${missingImplicits.mkString(", ")} available.")

      val combinedTree = effectiveInferredImplicits.foldLeft(q"")((z, e) => q"$z;implicitly[${e.neededImplicit.tpe}]")
      val finalTree = q"""
      $combinedTree
      val fakeType = new FakeType[${atag.tpe.typeSymbol}] {}
      fakeType
    """
      c.Expr[FakeType[A]](finalTree)
    }
  }

}
