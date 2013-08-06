/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestHelper
import tests.util.TestRefactoring

import language.reflectiveCalls

class PrependOrDropScalaPackageFromRecomputedTest extends OrganizeImportsBaseTest {

  def organizeDropScalaPackage(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.FullyRecompute, options = List(refactoring.DropScalaPackage))
  }.mkChanges

  def organizePrependScalaPackage(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.FullyRecompute, options = List(refactoring.PrependScalaPackage))
  }.mkChanges

  @Test
  def dropScalaPackageWildcardImport = new FileSet {
    """
    import scala.math.BigDecimal._

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """ becomes
    """
    import math.BigDecimal.apply

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """
  } applyRefactoring organizeDropScalaPackage

  @Test
  def prependScalaPackageWildcardImport = new FileSet {
    """
    import math.BigDecimal._

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """ becomes
    """
    import scala.math.BigDecimal.apply

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """
  } applyRefactoring organizePrependScalaPackage

  @Test
  def wildcardAndRename = new FileSet {
    """
    package tests.importing

    import scala.collection.mutable.{BitSet, ListBuffer => LB, _}

    object Main {val lb = LB(1) }
    """ becomes
    """
    package tests.importing

    import collection.mutable.{ListBuffer => LB}

    object Main {val lb = LB(1) }
    """
  } applyRefactoring organizeDropScalaPackage

  @Test
  def noScalaPackage = new FileSet {
    """
    import java.lang.{String, Math}

    object Main {val s: String = ""}
    """ becomes
    """
    import java.lang.String

    object Main {val s: String = ""}
    """
  } applyRefactoring organizePrependScalaPackage

  @Test
  def importFromPackageObject = new FileSet {
    """
    import scala.collection.breakOut
    import scala.collection.mutable.ListBuffer

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
      }
    """ becomes """
    import collection.breakOut

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
      }
    """
  } applyRefactoring organizeDropScalaPackage

  @Test
  def fromMixedToUniformDrop = new FileSet {
    """
    package fromMixedToUniformDrop
    import collection.immutable
    import scala.collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
      val n = new immutable.HashSet[String]
    }
    """ becomes
    """
    package fromMixedToUniformDrop

    import collection.immutable
    import collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
      val n = new immutable.HashSet[String]
    }
    """
  } applyRefactoring organizeDropScalaPackage

  @Test
  def fromMixedToUniformPrepend = new FileSet {
    """
    package fromMixedToUniformDrop
    import collection.immutable
    import scala.collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
      val n = new immutable.HashSet[String]
    }
    """ becomes
    """
    package fromMixedToUniformDrop

    import scala.collection.immutable
    import scala.collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
      val n = new immutable.HashSet[String]
    }
    """
  } applyRefactoring organizePrependScalaPackage
}
