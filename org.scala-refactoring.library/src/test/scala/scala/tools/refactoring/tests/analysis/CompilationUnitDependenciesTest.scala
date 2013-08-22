/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import analysis.CompilationUnitDependencies
import org.junit.Assert.assertEquals
import tests.util._
import org.junit.After
import common.TreeExtractors

class CompilationUnitDependenciesTest extends TestHelper with CompilationUnitDependencies with TreeExtractors with FreshCompilerForeachTest {

  import global._

  private def assertTrees(expected: String, src: String, f: Tree => Seq[Tree]) {
    val tree = treeFrom(src)
    val imports = f(tree).sortBy(_.toString).map(asString)
    assertEquals(expected.split("\n").map(_.trim).mkString("\n"), imports.mkString("\n"))
  }

  def assertNeededImports(expected: String, src: String): Unit = global.ask { () =>
    assertTrees(expected, src, neededImports)
  }

  def assertDependencies(expected: String, src: String): Unit = global.ask { () =>
    assertTrees(expected, src, dependencies)
  }

  @Test
  def evidenceNoImport = assertNeededImports(
    """""",
    """
    trait Transformations {

      abstract class Transformation[X, Y] {
        def apply(x: X): Option[Y]
      }

      def allChildren[X <% (X ⇒ Y) ⇒ Y, Y](t: ⇒ Transformation[X, Y]) = new Transformation[X, Y] {
        def apply(in: X): Option[Y] = {
          Some(in(child => t(child) getOrElse (return None)))
        }
      }
    }
    """)

  @Test
  def evidence = assertDependencies(
    """scala.Some""",
    """
    trait Transformations {

      abstract class Transformation[X, Y] {
        def apply(x: X): Option[Y]
      }

      def allChildren[X <% (X ⇒ Y) ⇒ Y, Y](t: ⇒ Transformation[X, Y]) = new Transformation[X, Y] {
        def apply(in: X): Option[Y] = {
          Some(in(child => t(child) getOrElse (return None)))
        }
      }
    }
    """)

  @Test
  def typeFromScalaPackage = assertDependencies(
    """""",
    """
       object NoRuleApplies extends Exception("No Rule Applies")
    """)

  @Test
  def abstractValType = assertDependencies(
    """java.util.Observable
       scala.collection.mutable.ListBuffer""",
    """
       import collection.mutable._
       import java.util._
       trait X {val lb: ListBuffer[Int]; val ob: Observable}
    """)

  @Test
  def dependencyOnMultipleOverloadedMethods = assertNeededImports(
    """scala.math.BigDecimal.apply""",
    """
      import scala.math.BigDecimal._

      class C {
        def m() {
          apply("5")
          apply(5l)
        }
      }
    """)

  @Test
  def typeArgument = assertDependencies(
    """scala.collection.mutable.ListBuffer""",
    """
       import collection.mutable._
       trait X {
         def m: Either[Int, Option[List[ListBuffer[ListBuffer[Int]]]]]
       }
    """)

  @Test
  def objectType = assertDependencies(
    """<root>.scala.io.Source""",
    """
      import scala.io._
      class MNO { var no: Source.type = null }
      """)

  @Test
  def objectTypeRequiresImport = assertNeededImports(
    """<root>.scala.io.Source""",
    """
      import scala.io._
      class MNO { var no: Source.type = null }
      """)

  @Test
  def valAnnotation = assertDependencies(
    """java.lang.Object
       scala.beans.BeanProperty
       scala.this.Predef.String""",
    """
      import scala.beans.BeanProperty
      case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
      """)

  @Test
  @ScalaVersion(matches="2.10.0")
  def switchAnnotation210 = assertDependencies(
    """Integer.parseInt
       java.this.lang.Integer
       scala.annotation.switch""",
    """
      import scala.annotation._
      object ASwitch {
        val x = (Integer.parseInt("5"): @switch) match {
          case 5 => true
          case 6 => false
        }
      }
      """)

  @Test
  @ScalaVersion(matches="2.10.1")
  def switchAnnotation2101 = assertDependencies(
    """java.this.lang.Integer
       scala.annotation.switch""",
    """
      import scala.annotation._
      object ASwitch {
        val x = (Integer.parseInt("5"): @switch) match {
          case 5 => true
          case 6 => false
        }
      }
      """)

  @Test
  @ScalaVersion(matches="2.11")
  def switchAnnotation = assertDependencies(
    """java.this.lang.Integer
       scala.annotation.switch""",
    """
      import scala.annotation._
      object ASwitch {
        val x = (Integer.parseInt("5"): @switch) match {
          case 5 => true
          case 6 => false
        }
      }
      """)

  @Test
  def annotationRequiresImport = assertNeededImports(
    """scala.beans.BeanProperty""",
    """
      import scala.beans.BeanProperty
      case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
      """)

  @Test
  def classAttributeDeps = assertDependencies(
    """scala.collection.mutable.Map
       scala.this.Predef.String""",
    """
      import scala.collection.mutable.Map
      class UsesMap { val x = Map[Int, String]() }
      """)

  @Test
  def classAttributeRequiresImport = assertNeededImports(
    """scala.collection.mutable.Map""",
    """
      import scala.collection.mutable.Map
      class UsesMap { val x = Map[Int, String]() }
    """)

  @Test
  def fullAndShortNames = assertNeededImports(
    """scala.collection.mutable.Map
       scala.collection.mutable.Set""",
    """
      import scala.collection.mutable.Map
      import scala.collection.mutable.Set
      class UsesMap {
        val x1 = Map[Int, String]()
        val x2 = scala.collection.mutable.Map[Int, String]()

        val y2 = scala.collection.mutable.Set[String]()
        val y1 = Set[String]()
      }
    """)

  @Test
  def renamedImport = assertDependencies(
    """scala.collection.mutable.Map
       scala.this.Predef.String""",
    """
      import scala.collection.mutable.{Map => M}
      class UsesMap { val x = M[Int, String]() }
      """)

  @Test
  def renamedImportIsNeeded = assertNeededImports(
    """scala.collection.mutable.Map""",
    """
      import scala.collection.mutable.{Map => M}
      class UsesMap { val x = M[Int, String]() }
    """)

  @Test
  def localImport = assertDependencies(
    """scala.this.Predef.println
       x.B""",
    """
      class A {
        val B = new {
          val y = 2
        }
      }

      object CLocalImport {
        def m(x: A) {
          import x._
          println(B.y)
        }
      }
      """)

  @Test
  def localImportNotNeeded = assertNeededImports(
    "",
    """
      class A {
        val B = new {
          val y = 2
        }
      }

      object ClocalImportNotNeeded {
        def m(x: A) {
          import x._
          println(B.y)
        }
      }
      """)

  @Test
  def classAttributeWithFullPackage = assertDependencies(
    """scala.collection.mutable.Map
       scala.this.Predef.String""",
    """
      class UsesMap { val x = collection.mutable.Map[Int, String]() }
      """)

  @Test
  def classAttributeWithFullPackageNoImports = assertNeededImports(
    "",
    """
      class UsesMap { val x = collection.mutable.Map[Int, String]() }
      """)

  @Test
  def etaExpandedMethod = assertNeededImports(
    "",
    """
      trait A {
        val x = Set()
        x filterNot (x ++ x contains)
      }      """)

  @Test
  def classAttributeWithWildcardImport = assertDependencies(
    """scala.collection.mutable.HashSet""",
    """
      import collection._
      class UsesMap { val x = mutable.HashSet[Int]() }
      """)

  @Test
  def classAttributeWithWildcardImportNeeded = assertNeededImports(
    """scala.collection.mutable""",
    """
      import collection._
      class UsesMap { val x = mutable.HashSet[Int]() }
      """)

  @Test
  def importIsUsedAsType = assertDependencies(
    """java.util.ArrayList""",
    """
      import java.util._
      class UsesMap { def x(m: java.util.ArrayList[Int]) = () }
      """)

  @Test
  def importIsUsedAsTypeButNotNeeded = assertNeededImports(
    "",
    """
      import java.util._
      class UsesMap { def x(m: java.util.ArrayList[Int]) = () }
      """)

  @Test
  @ScalaVersion(matches="2.10")
  def importIsUsedAsTypeAscription210 = assertDependencies(
    """scala.collection.immutable.Set
       scala.this.Predef.Map
       scala.this.Predef.any2ArrowAssoc""",
    """
      class UsesSet { val s: collection.immutable.Set[Any] = Map(1 -> 2).toSet }
      """)

  @Test
  @ScalaVersion(matches="2.11")
  def importIsUsedAsTypeAscription = assertDependencies(
    """scala.collection.immutable.Set
       scala.this.Predef.ArrowAssoc
       scala.this.Predef.Map""",
    """
      class UsesSet { val s: collection.immutable.Set[Any] = Map(1 -> 2).toSet }
      """)

  @Test
  def importIsUsedAsTypeAscriptionNeeded = assertNeededImports(
    "",
    """
      class UsesSet { val s: collection.immutable.Set[Any] = Map(1 -> 2).toSet }
      """)

  @Test
  def typeUsedAsParent = assertDependencies(
    """java.util.Observer""",
    """
      import java.util.Observer
      trait X extends Observer
      """)

  @Test
  def typeUsedAsParentImportNeeded = assertNeededImports(
    """java.util.Observer""",
    """
      import java.util.Observer
      trait X extends Observer
      """)

  @Test
  def singleTypeUsedAsSelfTypeAnnotation = assertDependencies(
    """java.util.Observer""",
    """
      package singleTypeUsedAsSelfTypeAnnotation
      import java.util.Observer
      trait X {
        this: Observer =>
      }
      """)

  @Test
  def singleTypeUsedAsSelfTypeAnnotationImport = assertNeededImports(
    """java.util.Observer""",
    """
      import java.util.Observer
      trait X {
        self: Observer =>
      }
      """)

  @Test
  def typeUsedAsSelfTypeAnnotation = assertDependencies(
    """java.util.Observable
         java.util.Observer""",
    """
      import java.util.Observer
      import java.util.Observable
      trait Y
      trait X {
        this: Observer with Observable with Y =>
      }
      """)

  @Test
  def typeUsedAsSelfTypeAnnotationImportsNeeded = assertNeededImports(
    """java.util.Observable
         java.util.Observer""",
    """
      import java.util.Observer
      import java.util.Observable
      trait Y
      trait X {
        this: Observer with Observable with Y =>
      }
      """)

  @Test
  def compoundTypeTree = assertDependencies(
    """java.util.Observable
         java.util.Observer""",
    """
      import java.util.Observer
      trait X {
        def x(y: Observer with java.util.Observable) = ()
      }
      """)

  @Test
  def typeUsedAsBound = assertDependencies(
    """java.util.Observer""",
    """
      import java.util.Observer
      class X[T <: Observer] {
      }
      """)

  @Test
  def typeUsedAsBoundNeeded = assertNeededImports(
    """java.util.Observer""",
    """
      import java.util.Observer
      class X[T <: Observer] {
      }
      """)

  @Test
  def qualifiedAndUnqualifiedImports = assertNeededImports(
    """scala.collection.mutable.HashMap""",
    """
      import collection.mutable.HashMap

      trait A {
        val x = new HashMap[String, String]
        val y = new collection.mutable.HashMap[String, String]
      }
      """)

  @Test
  def importStaticMethodDependency = assertDependencies(
    """java.lang.Integer.parseInt""",
    """
      import java.lang.Integer._
      class Y {
        val s = parseInt("5")
      }
      """)

  @Test
  def importStaticMethodNeeded = assertNeededImports(
    """java.lang.Integer.parseInt""",
    """
      import java.lang.Integer._
      class Y {
        val s = parseInt("5")
      }
      """)

  @Test
  def typeUsedInNew = assertDependencies(
    """scala.this.Predef.intWrapper
       scala.util.Random""",
    """
      import scala.util._
      class X {
        val r = new Random
        (0 to 10) map (_ => r.nextInt) sum
      }
      """)

  @Test
  def typeUsedInNewNeeded = assertNeededImports(
    """scala.util.Random""",
    """
      import scala.util._
      class X {
        val r = new Random
        (0 to 10) map (_ => r.nextInt) sum
      }
      """)

  @Test
  def dependenciesAreUnique = assertDependencies(
    """scala.collection.mutable.ListBuffer""",
    """
      import scala.collection.mutable.ListBuffer
      trait DependenciesAreUnique {val x: ListBuffer[Int]; val y: ListBuffer[Int]}
      """)

  @Test
  def existential = assertDependencies(
    """java.util.Map""",
    """
      import java.util._
      trait Y {
        def build(ignored: Map[_, _])
      }
      """)

  @Test
  def existentialTypeTree = assertNeededImports(
    """java.util.Map""",
    """
      import java.util._
      trait Y {
        def build(ignored: Map[_, _])
      }
      """)

  @Test
  def renamedPackage = assertDependencies(
    """java.util.Map""",
    """
      import java.{ lang => jl, util => ju }
      trait Y {
        def build(ignored : ju.Map[_, _])
      }
      """)

  @Test
  def renamedPackageImport = assertNeededImports(
    """java.util""",
    """
      import java.{ lang => jl, util => ju }
      trait Y {
        def build(ignored : ju.Map[_, _])
      }
      """)

  @Test
  def importFromPackageObject = assertDependencies(
    """scala.collection.`package`.breakOut
       scala.this.Predef.Map
       scala.this.Predef.identity""",
    """
      import scala.collection.breakOut
      object TestbreakOut {
        val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
      }
      """)

  @Test
  def selfTypeFromThis = assertDependencies(
    """""",
    """
      trait A {
        trait A2
      }
      class C extends A {
        trait C2 {
          this: A2 =>
        }
      }
      """)

  @Test
  def somePackages = assertDependencies(
    """a.X""",
    """
      package a {
        trait X
      }

      package b {
        trait Y extends a.X
      }
      """)

  @Test
  def somePackagesButNoImports = assertNeededImports(
    "",
    """
      package a {
        trait X
      }

      package b {
        trait Y extends a.X
      }
      """)

  @Test
  def importedImplicitConversion = assertDependencies(
    """java.util.List
       scala.collection.JavaConversions.bufferAsJavaList
       scala.collection.mutable.ListBuffer""",
    """
      import scala.collection.JavaConversions._
      object Conversions {
        val sl = new scala.collection.mutable.ListBuffer[Int]
        val jl : java.util.List[Int] = sl
      }
      """)

  @Test
  def importedImplicitConversionNeedsImport = assertNeededImports(
    """scala.collection.JavaConversions.bufferAsJavaList""",
    """
      import scala.collection.JavaConversions._
      object Conversions {
        val sl = new scala.collection.mutable.ListBuffer[Int]
        val jl : java.util.List[Int] = sl
      }
      """)

  @Test
  def importedImplicitConversionNeedsImportShortForm = assertNeededImports(
    """scala.collection.JavaConversions.asScalaBuffer""",
    """
      import collection.JavaConversions._
      class ListConversion {
        val l = new java.util.ArrayList[String]
        l map (_.toInt)
      }
    """)

  @Test
  def importedImplicitArgument {

    addToCompiler("xy.scala", """
      package impl.args
      object Implicits {
        implicit val x = "<empty>"
      }
    """)

    assertNeededImports(
    """impl.args.Implicits.x""",
    """
      import impl.args.Implicits._
      object Conversions {
        def doWithImpl(a: Int)(implicit s: String) = s
        doWithImpl(5)
      }
      """)

    assertDependencies(
    """impl.args.Implicits.x
       scala.this.Predef.String""",
    """
      import impl.args.Implicits._
      object Conversions {
        def doWithImpl(a: Int)(implicit s: String) = s
        doWithImpl(5)
      }
      """)
  }

  @Test
  def importFromLocalValueDependency = assertDependencies(
    """param.global.X""",
    """
      trait Param {
        object global { trait X }
      }
      trait SomeTrait {
        def method(param: Param) {
          import param._
          import global._
          var x: X = null
        }
      }
    """)

  @Test
  def importFromLocalValueNoImport = assertNeededImports(
    """""",
    """
      trait Param {
        object global { trait X }
      }
      trait SomeTrait {
        def method(param: Param) {
          import param._
          import global._
          var x: X = null
        }
      }
    """)

  @Test
  def classOfRequiresImport = assertNeededImports(
    """scala.io.Source""",
    """
      import scala.io.Source

      object Dummy {
        val clazz = classOf[Source]
      }
    """)

  @Test
  def SystemcurrentTimeMillis = assertNeededImports(
    """java.this.lang.System.currentTimeMillis""",
    """
      import System.currentTimeMillis

      object Dummy {
        val x = currentTimeMillis
      }
    """)

  @Test
  def SystemcurrentTimeMillisDeps = assertDependencies(
    """java.this.lang.System.currentTimeMillis""",
    """
      import System.currentTimeMillis

      object Dummy {
        val x = currentTimeMillis
      }
    """)

  @Test
  def ClassInAnnotationAttrArg = assertNeededImports(
    """java.util.Calendar""",
    """
      import java.util.Calendar

      class DeprecatedAnnotation {
        @deprecated(message = "", since = ""+ Calendar.YEAR)
       def read() = ()
      }
    """)

  @Test
  def ClassInAnnotationImports = assertNeededImports(
    """java.io.BufferedReader
       java.io.FileReader
       java.io.IOException""",
    """
      package examples

      import java.io._

      class Reader(fname: String) {
        private val in = new BufferedReader(new FileReader(fname))
        @throws(classOf[IOException])
        def read() = in.read()
      }
    """)

  @Test
  def ClassInAnnotationDeps = assertDependencies(
    """java.io.BufferedReader
       java.io.FileReader
       java.io.IOException
       scala.this.Predef.String""",
    """
      package examples

      import java.io._

      class Reader(fname: String) {
        private val in = new BufferedReader(new FileReader(fname))
        @throws(classOf[IOException])
        def read() = in.read()
      }
    """)

  @Test
  def implicitDefImports  = assertNeededImports(
    "", """class ImplicitDef {

      val readBuffer = Array.ofDim[Byte](1024)
      val dataId: (Byte, Byte) = readBuffer.slice(0, 2)

      implicit def arrayTo2Tuple[T](a: Array[T]): (T, T) = {
        (a(0), a(1))
      }
    }""")

  @Test
  def implicitDef = assertDependencies(
    """ClassTag.Byte
       scala.this.Predef.byteArrayOps""",
    """class ImplicitDef {

      val readBuffer = Array.ofDim[Byte](1024)
      val dataId: (Byte, Byte) = readBuffer.slice(0, 2)

      implicit def arrayTo2Tuple[T](a: Array[T]): (T, T) = {
        (a(0), a(1))
      }
    }""")

  @Test
  def annotationOnPrimaryConstructor = assertDependencies(
    "java.lang.annotation.Documented",
    """
      import java.lang.annotation.Documented

      class Foo @Documented() (i: Int)
    """)


  @Test
  def annotationOnField = assertDependencies(
    "java.lang.annotation.Documented",
    """
      import java.lang.annotation.Documented

      class Foo {
        @Documented()
        def xx(i: Int) = i
      }
    """)
}

