/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestHelper

import language.reflectiveCalls

class OrganizeImportsFullyRecomputeTest extends OrganizeImportsBaseTest {

  def organize(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(deps = refactoring.Dependencies.FullyRecompute)
  }.mkChanges

  def organizeWithoutCollapsing(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(options = List(), deps = refactoring.Dependencies.FullyRecompute)
  }.mkChanges

  def organizeExpand(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    val params = new RefactoringParameters(options = List(refactoring.ExpandImports), deps = refactoring.Dependencies.FullyRecompute)
  }.mkChanges

  @Test
  def testOrganizeOptions() {

    val src = """
      package tests.importing

      import scala.collection.mutable.{ListBuffer, HashMap}
      import scala.io.Source
      import scala.math.BigInt
      import scala.math._

      import scala.util.{Properties => ScalaProperties}
      """

    val restOfFile = """
      object Main {
        // we need to actually use the imports, otherwise they are removed
        val lb = ListBuffer(1)
        val lb = HashMap(1 → 1)
        var no: Source.type = null
        var elem: Source = null
        var bigi: BigInt = null
        var bigd: BigDecimal = null
        var props: ScalaProperties = null
      }
      """

    new FileSet(expectCompilingCode = false) {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source
      import scala.io.Source
      import scala.math.BigDecimal
      import scala.math.BigInt
      """ + restOfFile
    } applyRefactoring organizeWithoutCollapsing

    new FileSet(expectCompilingCode = false) {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source
      import scala.io.Source
      import scala.math.BigDecimal
      import scala.math.BigInt
      """ + restOfFile
    } applyRefactoring organizeExpand

    new FileSet(expectCompilingCode = false) {
      (src + restOfFile) becomes
      """
      package tests.importing

      import scala.collection.mutable.{HashMap, ListBuffer}
      import scala.io.Source
      import scala.math.{BigDecimal, BigInt}
      """ + restOfFile
    } applyRefactoring organize
  }

  @Test
  def dependencyOnMultipleOverloadedMethods() = new FileSet {
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
    import scala.math.BigDecimal.apply

    class C {
      def m() {
        apply("5")
        apply(5l)
      }
    }
    """
  } applyRefactoring organize

  @Test
  def expandImportsButNotWildcards() = new FileSet {
    """
    package tests.importing

    import scala.collection.mutable.{ListBuffer => LB, _}

    object Main {val lb = LB(1) }
    """ becomes
    """
    package tests.importing

    import scala.collection.mutable.{ListBuffer => LB}

    object Main {val lb = LB(1) }
    """
  } applyRefactoring organizeExpand

  @Test
  def dontCollapseImports() = new FileSet {
    """
    package tests.importing

    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashMap

    object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """ becomes
    """
    package tests.importing

    import scala.collection.mutable.HashMap
    import scala.collection.mutable.ListBuffer

    object Main {val lb = ListBuffer(1); val lb = HashMap(1 → 1) }
    """
  } applyRefactoring organizeWithoutCollapsing

  @Test
  def collapse() = new FileSet {
    """
    import scala.collection.mutable.Set
    import scala.collection.mutable.Queue

    object Main {val s: Set[_] = null; var q: Queue[_] = null}
    """ becomes
    """
    import scala.collection.mutable.{Queue, Set}

    object Main {val s: Set[_] = null; var q: Queue[_] = null}
    """
  } applyRefactoring organize

  @Test
  def sortSelectors() = new FileSet {
    """
    import scala.collection.mutable.{Set, Queue}

    object Main {val s: Set[_] = null; var q: Queue[_] = null}
    """ becomes
    """
    import scala.collection.mutable.{Queue, Set}

    object Main {val s: Set[_] = null; var q: Queue[_] = null}
    """
  } applyRefactoring organize

  @Test
  def sortAndCollapse() = new FileSet {
    """
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.Set
    import scala.collection.mutable.Queue

    object Main {val s: Set[_] = null; var q: Queue[_] = null; val lb = ListBuffer(1)}
    """ becomes
    """
    import scala.collection.mutable.{ListBuffer, Queue, Set}

    object Main {val s: Set[_] = null; var q: Queue[_] = null; val lb = ListBuffer(1)}
    """
  } applyRefactoring organize

  @Test
  def collapseWithRename() = new FileSet {
    """
    import collection.mutable.{Set => S}
    import collection.mutable.{Queue => Q}

    object Main {val s: S[_] = null; var o: Q[_] = null}
    """ becomes
    """
    import scala.collection.mutable.{Queue => Q, Set => S}

    object Main {val s: S[_] = null; var o: Q[_] = null}
    """
  } applyRefactoring organize

  @Test
  def removeOneFromMany() = new FileSet {
    """
    import collection.mutable.{HashSet, Queue}

    object Main {val s: HashSet[_] = null}
    """ becomes
    """
    import scala.collection.mutable.HashSet

    object Main {val s: HashSet[_] = null}
    """
  } applyRefactoring organize

  @Test
  def importAll() = new FileSet {
    """
    import java.lang._
    import java.lang.String

    object Main
    """ becomes
    """
    ▒

    object Main
    """
  } applyRefactoring organize

  @Test
  def importOnTrait() = new FileSet {
    """
    package importOnTrait
    import java.lang._
    import java.lang.String

    trait A

    trait Main extends A {
    }
    """ becomes
    """
    package importOnTrait

    trait A

    trait Main extends A {
    }
    """
  } applyRefactoring organize

  @Test
  def importWithSpace() = new FileSet {
    """

    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.Seq

    object Main { val s: Seq[_] = null; val lb = ListBuffer("") }
  """ becomes
  """

    import scala.collection.mutable.{ListBuffer, Seq}

    object Main { val s: Seq[_] = null; val lb = ListBuffer("") }
  """
  } applyRefactoring organize

  @Test
  def importAllWithRename() = new FileSet {
    """
    import collection.mutable._
    import collection.mutable.{Set => S}

    object Main { val s: Set[_] = null }
    """ becomes
    """
    import scala.collection.mutable.Set

    object Main { val s: Set[_] = null }
    """
  } applyRefactoring organize

  @Test
  def importRemovesUnneeded() = new FileSet {
    """
    import java.lang._
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
  def multipleImportsOnOneLine() = new FileSet {
    """
    import java.lang.String, String._

    object Main {
      val s: String = ""
      val s1 = valueOf(2);
    }    """ becomes """
    import java.lang.String.valueOf

    object Main {
      val s: String = ""
      val s1 = valueOf(2);
    }    """
  } applyRefactoring organize

  @Test
  def importsInNestedPackages() = new FileSet {
    """
    package outer
    package inner

    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashMap

    object Main {
      var hm: HashMap[String, String] = null
    }
    """ becomes """
    package outer
    package inner

    import scala.collection.mutable.HashMap

    object Main {
      var hm: HashMap[String, String] = null
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
  def unusedImportWildcards() = new FileSet {
    """
    import java.util._
    import scala.collection._

    object Main {
    }    """ becomes
    """
    ▒

    object Main {
    }    """
  } applyRefactoring organize

  @Test
  def simplifyWildcards() = new FileSet {
    """
    import scala.collection.mutable._
    import scala.collection.mutable.ListBuffer

    object Main {
      var x: ListBuffer[Int] = null
    }    """ becomes """
    import scala.collection.mutable.ListBuffer

    object Main {
      var x: ListBuffer[Int] = null
    }    """
  } applyRefactoring organize

  @Test
  def appliedType() = new FileSet {
    """
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.ListBuffer

    trait SomeTrait {
      def m: Either[String, ListBuffer[ListBuffer[String]]]
    }    """ becomes
    """
    import scala.collection.mutable.ListBuffer

    trait SomeTrait {
      def m: Either[String, ListBuffer[ListBuffer[String]]]
    }    """
  } applyRefactoring organize

  @Test
  def annotation() = new FileSet {
    """
    import scala.beans.BeanProperty
    case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
    """ becomes
    """
    import scala.beans.BeanProperty
    case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
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
  def abstractVals() = new FileSet {
    """
    import scala.collection.mutable.ListBuffer
    import scala.collection._

    trait Temp {
      // we need some code that use the imports
      val x: (ListBuffer[Int], mutable.HashMap[String, Int])
      }
    """ becomes
    """
    import scala.collection.mutable
    import scala.collection.mutable.ListBuffer

    trait Temp {
      // we need some code that use the imports
      val x: (ListBuffer[Int], mutable.HashMap[String, Int])
      }
    """
  } applyRefactoring organize

  @Test
  def fullPaths() = new FileSet {
    """
    trait FullPaths {
      sys.error("")
      math.E
    }
    """ becomes
    """
    trait FullPaths {
      sys.error("")
      math.E
    }
    """
  } applyRefactoring organize

  @Test
  def organizeNeededForTypeInClassOf() = new FileSet {
    """
    import scala.io.Source

    object Dummy {
      val clazz = classOf[Source]
    }
    """ becomes
    """
    import scala.io.Source

    object Dummy {
      val clazz = classOf[Source]
    }
    """
  } applyRefactoring organize

  @Test
  def SystemcurrentTimeMillis() = new FileSet {
    """
    import System.currentTimeMillis

    object Dummy {
      val x = currentTimeMillis
    }
    """ becomes
    """
    import java.lang.System.currentTimeMillis

    object Dummy {
      val x = currentTimeMillis
    }
    """
  } applyRefactoring organize

  @Test
  def dontImportSystem() = new FileSet {
    """
    class SystemTypeDef {
      type S = System
    }
    """ becomes
    """
    class SystemTypeDef {
      type S = System
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

    import TestImplicits.stringToBytes

    object Tester {
      "":Array[Byte]
    }
    """
  } applyRefactoring organize

  @Test
  def importedPackageHasKeywordName() = new FileSet {

    addToCompiler("testkeyword", """
    package other
    package `type`
    object `implicit` {
      val x = 42
    }""");

    """
    package a.b.c
    import other.`type`.`implicit`

    object Tester {
      val x = `implicit`.x
    }
    """ becomes
    """
    package a.b.c

    import other.`type`.`implicit`

    object Tester {
      val x = `implicit`.x
    }
    """
  } applyRefactoring organize

  @Test
  def fileWithoutNewline() = new FileSet {
    """
    import java.util.Date
    class MyClass[T]""" becomes
    """
    ▒
    class MyClass[T]"""
  } applyRefactoring organize

  @Test
  def parensAtEndOfFile() = new FileSet {
    """
    import java.util.Date
    class MyClass(i: Int)""" becomes
    """
    ▒
    class MyClass(i: Int)"""
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
  def importWithSelfType() = new FileSet {
    """
    package importWithSelfType

    import java.util.Observable

    trait Coccccc {
      this: Observable =>

      def eval(ctx: String, t: Int): Int = try {
        42
      } catch {
        case _ => t
      }
    }
    """ becomes
    """
    package importWithSelfType

    import java.util.Observable

    trait Coccccc {
      this: Observable =>

      def eval(ctx: String, t: Int): Int = try {
        42
      } catch {
        case _ => t
      }
    }
    """
  } applyRefactoring organize

  @Test
  def collapseTypes() = new FileSet {
    """
    import scala.util.DynamicVariable
    import scala.util.Random

    trait Bogus {
      def a: Random
      def b: DynamicVariable[_]
    }
    """ becomes
    """
    import scala.util.{DynamicVariable, Random}

    trait Bogus {
      def a: Random
      def b: DynamicVariable[_]
    }
    """
  } applyRefactoring organize

  @Test
  def typeConstructors() = new FileSet {
    """
    trait Property[+T]

    class A {
      type Prop_Tp[+Vl_Tpe] <: Property[Vl_Tpe]
      def properties: Set[Prop_Tp[_]] = null.asInstanceOf[Set[Prop_Tp[_]]]
    }
    """ becomes
    """
    trait Property[+T]

    class A {
      type Prop_Tp[+Vl_Tpe] <: Property[Vl_Tpe]
      def properties: Set[Prop_Tp[_]] = null.asInstanceOf[Set[Prop_Tp[_]]]
    }
    """
  } applyRefactoring organize

  @Test
  @ScalaVersion(matches="2.10")
  def annotationTypeDef() = new FileSet {

    addToCompiler("ann.scala", """
      package pkg
      object annotations {
        type Doc = java.lang.annotation.Documented
      }
    """)

    """
    package whatever
    import pkg.annotations.Doc

    @Doc
    class Documented
    """ becomes
    """
    package whatever

    import pkg.annotations.Doc

    @Doc
    class Documented
    """
  } applyRefactoring organize

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
  def implicitConversion() = new FileSet {
    addToCompiler("implicitConversion.scala", """
      package implicitConversion
      trait AimplicitConversion {
        class B
        object B {
          implicit def any2B(i: Any): B = new B
        }
      }
    """);
    """
    import implicitConversion.AimplicitConversion

    object C extends AimplicitConversion {
      val b: B = new Object
    }
    """ becomes
    """
    import implicitConversion.AimplicitConversion

    object C extends AimplicitConversion {
      val b: B = new Object
    }
    """
  } applyRefactoring organize

  @Test
  @Ignore // fails when run by Jenkins
  def dependencyInSameFile() = new FileSet {
    """
    package dependencyInSameFile
    class Foo {
      import Bar.instance

      def foo = instance.toString
      def bar = Bar.instance.toString
    }

    object Bar {
      val instance = new Bar
    }

    class Bar
    """ becomes
    """
    package dependencyInSameFile
    class Foo {
      import Bar.instance

      def foo = instance.toString
      def bar = Bar.instance.toString
    }

    object Bar {
      val instance = new Bar
    }

    class Bar
    """
  } applyRefactoring organize

  @Test
  def ignoreScalaLanguageImports() = new FileSet {
    """
    package ignoreScalaLanguageImports

    import scala.language.reflectiveCalls

    object Bar {
    }
    """ becomes
    """
    package ignoreScalaLanguageImports

    import scala.language.reflectiveCalls

    object Bar {
    }
    """
  } applyRefactoring organize

  @Test
  def ignoreLanguageImports() = new FileSet {
    """
    package ignoreScalaLanguageImports

    import language.reflectiveCalls

    object Bar {
    }
    """ becomes
    """
    package ignoreScalaLanguageImports

    import scala.language.reflectiveCalls

    object Bar {
    }
    """
  } applyRefactoring organize

  @Test
  def runWith() = new FileSet {
    """
    package runWith

    class RunWith(c: Class[_]) extends scala.annotation.StaticAnnotation

    import java.util.BitSet

    @RunWith(classOf[BitSet])
    class MainActivityTest {}
    """ becomes
    """
    package runWith

    import java.util.BitSet

    class RunWith(c: Class[_]) extends scala.annotation.StaticAnnotation

    @RunWith(classOf[BitSet])
    class MainActivityTest {}
    """
  } applyRefactoring organize

  @Test
  def shouldIgnoreScalaPackage() = new FileSet {
    """
    package shouldIgnoreScalaPackage

    class Blah {  val x = for (e <- Array(1, 2, 3)) yield e }
    """ becomes
    """
    package shouldIgnoreScalaPackage

    class Blah {  val x = for (e <- Array(1, 2, 3)) yield e }
    """
  } applyRefactoring organize
}
