/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import org.junit.Test

import language.reflectiveCalls

class OrganizeImportsWildcardsTest extends OrganizeImportsBaseTest {

  def organize(groups: Set[String])(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    import refactoring._
    val options = List(AlwaysUseWildcards(groups), ExpandImports, SortImports)
    val params = new RefactoringParameters(options = options, deps = Dependencies.FullyRecompute)
  }.mkChanges

  val source = """
    import scala.collection.mutable.Set
    import org.xml.sax.Attributes
    import Set._

    trait Temp {
      // we need some code that use the imports
      val z: (Attributes, Set[String])
      println(apply(""))
    }
    """

  @Test
  def noGrouping = new FileSet {
    source becomes
    """
    import org.xml.sax.Attributes
    import scala.collection.mutable.Set
    import scala.collection.mutable.Set.apply

    trait Temp {
      // we need some code that use the imports
      val z: (Attributes, Set[String])
      println(apply(""))
    }
    """
  } applyRefactoring organize(Set())

  @Test
  def simpleWildcard = new FileSet {
    source becomes
    """
    import org.xml.sax.Attributes
    import scala.collection.mutable.Set
    import scala.collection.mutable.Set._

    trait Temp {
      // we need some code that use the imports
      val z: (Attributes, Set[String])
      println(apply(""))
    }
    """
  } applyRefactoring organize(Set("scala.collection.mutable.Set"))

  @Test
  def renamedImport = new FileSet {
    """
    import java.lang.Integer.{valueOf => vo}
    import java.lang.Integer.toBinaryString
    import java.lang.String.valueOf

    trait Temp {
      valueOf(5)
      vo("5")
      toBinaryString(27)
    }
    """ becomes
    """
    import java.lang.Integer._
    import java.lang.Integer.{valueOf => vo}
    import java.lang.String.valueOf

    trait Temp {
      valueOf(5)
      vo("5")
      toBinaryString(27)
    }
    """
  } applyRefactoring organize(Set("java.lang.Integer"))

  @Test
  def multipleImportsOneWildcard = new FileSet {
    """
    import java.lang.Integer.valueOf
    import java.lang.Integer.toBinaryString
    import java.lang.Double.toHexString

    trait Temp {
      valueOf("5")
      toBinaryString(27)
      toHexString(5)
    }
    """ becomes
    """
    import java.lang.Double.toHexString
    import java.lang.Integer._

    trait Temp {
      valueOf("5")
      toBinaryString(27)
      toHexString(5)
    }
    """
  } applyRefactoring organize(Set("java.lang.Integer"))

}
