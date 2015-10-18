/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestHelper

import language.reflectiveCalls

class OrganizeImportsRecomputeAndModifyTest extends OrganizeImportsBaseTest {

  def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.RecomputeAndModify, options = List())
  }.mkChanges

  def organizeCleanup(groups: List[String])(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.RecomputeAndModify,
        options = List(refactoring.SortImports, refactoring.GroupImports(groups)))
  }.mkChanges

  def organizeWildcards(ws: Set[String])(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.RecomputeAndModify,
        options = List(refactoring.SortImports, refactoring.AlwaysUseWildcards(ws)))
  }.mkChanges

  @Test
  def wildcardImportIsNotExpanded() = new FileSet {
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
    import scala.math.BigDecimal._

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """
  } applyRefactoring organize

  @Test
  def wildcardAndRename() = new FileSet {
    """
    package tests.importing

    import scala.collection.mutable.{BitSet, ListBuffer => LB, _}

    object Main {val lb = LB(1) }
    """ becomes
    """
    package tests.importing

    import scala.collection.mutable.{ListBuffer => LB, _}

    object Main {val lb = LB(1) }
    """
  } applyRefactoring organize

  @Test
  def removeOneFromMany() = new FileSet {
    """
    import collection.mutable.{HashSet, Queue}

    object Main {val s: HashSet[_] = null}
    """ becomes
    """
    import collection.mutable.HashSet

    object Main {val s: HashSet[_] = null}
    """
  } applyRefactoring organize

  @Test
  def wildcardImportIsPreserved() = new FileSet {
    """
    import java.lang._
    import java.lang.String

    object Main
    """ becomes
    """
    import java.lang._

    object Main
    """
  } applyRefactoring organize

  @Test
  def sortAndGroup() = new FileSet {
    """
    import scala.collection.mutable.ListBuffer
    import java.util.Map

    object Main extends Map[String, String] {
      val l = ListBuffer(1,2,3)

    }
    """ becomes
    """
    import java.util.Map

    import scala.collection.mutable.ListBuffer

    object Main extends Map[String, String] {
      val l = ListBuffer(1,2,3)

    }
    """
  } applyRefactoring organizeCleanup(List("java", "scala"))

  @Test
  def importRemovesUnneeded() = new FileSet {
    """
    import java.lang.{String => S}
    import java.util.Map
    import scala.io.Source
    import scala.collection.mutable.ListBuffer

    object Main {
      val s: String = ""
      val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """ becomes
    """
    import scala.collection.mutable.ListBuffer

    object Main {
      val s: String = ""
      val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
      }
    """
  } applyRefactoring organize

  @Test
  def importFromPackageObject() = new FileSet {
    """
    import scala.collection.breakOut
    import scala.collection.mutable.ListBuffer

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
      }
    """ becomes """
    import scala.collection.breakOut

    object TestbreakOut {
      val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
      }
    """
  } applyRefactoring organize

  @Test
  def selfTypeAnnotation() = new FileSet {
    """
    import java.util.Observer
    trait X {
      self: Observer =>
    }
    """ becomes
    """
    import java.util.Observer
    trait X {
      self: Observer =>
    }
    """
  } applyRefactoring organize

  @Test
  def renamedPackage() = new FileSet {
    """
    import java.{ lang => jl, util => ju }
    import ju.{ArrayList => AL}
    trait Y {
      def build(ignored : ju.Map[_, _])
        def build2(ignored : AL[Int])
      }
    """ becomes
    """
    import java.{util => ju}
    import java.util.{ArrayList => AL}
    trait Y {
      def build(ignored : ju.Map[_, _])
        def build2(ignored : AL[Int])
      }
    """
  } applyRefactoring organize

  @Test
  def SystemcurrentTimeMillis() = new FileSet {
    """
    import System.currentTimeMillis
    import scala.collection.mutable.ListBuffer

    object Dummy {
      val x = currentTimeMillis
    }
    """ becomes
    """
    import System.currentTimeMillis

    object Dummy {
      val x = currentTimeMillis
    }
    """
  } applyRefactoring organize

  @Test
  def importMethodFromSamePackage() = new FileSet {

    addToCompiler("testimplicits", """
    package a.b.c
    object TestImplicits {
      implicit def stringToBytes(s: String): Array[Byte] = s.getBytes
    }""");

    """
    package a.b.c
    import TestImplicits._

    object Tester {
      "":Array[Byte]
    }
    """ becomes
    """
    package a.b.c
    import TestImplicits._

    object Tester {
      "":Array[Byte]
    }
    """
  } applyRefactoring organize

  @Test
  def importFromSamePackage() = new FileSet {

    addToCompiler("first", """
    package mypackage

    class First
    """);

    """
    package mypackage

    class Second {
      println(new First)
      println(classOf[First])
    }
    """ becomes
    """
    package mypackage

    class Second {
      println(new First)
      println(classOf[First])
    }
    """
  } applyRefactoring organize

  @Test
  def importFromSameNestedPackage() = new FileSet {

    addToCompiler("first", """
    package mypackage
    package sub

    class First
    """);

    """
    package mypackage
    package sub

    class Second {
      println(new First)
      println(classOf[First])
    }
    """ becomes
    """
    package mypackage
    package sub

    class Second {
      println(new First)
      println(classOf[First])
    }
    """
  } applyRefactoring organize

  @Test
  def severalScalaGroups() = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.util.BitSet
      import scala.io.Source
      import java.util.{AbstractList, SortedSet}
      import java.util.TreeSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet, TreeSet[Int])
        val z: (Attributes, Source)
      }
    """ becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import java.util.TreeSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import scala.io.Source

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet, TreeSet[Int])
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organizeCleanup(List("java", "scala.collection", "scala.io"))

  @Test
  def qualifiedImportFromPackageObject() = new FileSet {
    addToCompiler("package.scala", """
      package test

      package object pkg {
        def f_pkg() = 1
      }
    """)

    """
      package test2

      import test.pkg

      class ScalaClass {
        def f() {
          pkg.f_pkg
        }
      }
    """ becomes
    """
      package test2

      import test.pkg

      class ScalaClass {
        def f() {
          pkg.f_pkg
        }
      }
    """
  } applyRefactoring organize

  @Test
  def importDependingOnImport() = new FileSet {
    addToCompiler("Bar.scala", """
    package barr

    object Bar {
      val instance = new Bar

      def withInstance(f: Bar => Unit): Unit = ()
    }

    class Bar
    """)

    addToCompiler("Baz.scala", """
    package barr

    object Baz {
      def baz = 2
    }
    """)

    """
    package importDependingOnImport
    import barr.{Baz, Bar}
    import Bar.withInstance

    class Foo {
      Baz.baz
      withInstance { _ => () }
    }
    """ becomes
    """
    package importDependingOnImport

    import barr.Baz
    import barr.Bar.withInstance

    class Foo {
      Baz.baz
      withInstance { _ => () }
    }
    """
  } applyRefactoring organize

  @Test
  def removeDuplicate() = new FileSet {
    """
    package removeDuplicate
    import collection.mutable
    import collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
    }
    """ becomes
    """
    package removeDuplicate
    import collection.mutable

    class Foo {
      val m = new mutable.HashSet[String]
    }
    """
  } applyRefactoring organize
}
