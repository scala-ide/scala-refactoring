/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import analysis.CompilationUnitDependencies
import org.junit.Assert.assertEquals
import tests.util.TestHelper

class CompilationUnitDependenciesTest extends TestHelper with CompilationUnitDependencies with common.TreeExtractors {

  import global._
  
  private def assertTrees(expected: String, src: String, f: Tree => Seq[Tree]) {
    val tree = treeFrom(src)
    val imports = f(tree).sortBy(_.toString)
    assertEquals(expected.split("\n").map(_.trim).mkString("\n"), imports.mkString("\n")) 
  }
  
  def assertNeededImports(expected: String, src: String) {
    assertTrees(expected, src, neededImports)
  }

  def assertDependencies(expected: String, src: String) {
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
    """scala.xml.QNode""",
    """
      import scala.xml._
      class MNO { var no: QNode.type = null }
      """)

  @Test
  def objectTypeRequiresImport = assertNeededImports(
    """scala.xml.QNode""", 
    """
      import scala.xml._
      class MNO { var no: QNode.type = null }
      """)

  @Test
  def valAnnotation = assertDependencies(
    """java.lang.Object
       scala.reflect.BeanProperty""",
    """
      import scala.reflect.BeanProperty
      case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
      """)
      
  @Test
  def switchAnnotation = assertDependencies(
    """Integer.parseInt
       scala.annotation.switch""",
    """
      import scala.annotation._
      object A {
        val x = (Integer.parseInt("5"): @switch) match {
          case 5 => true
          case 6 => false
        }
      }
      """)

  @Test
  def annotationRequiresImport = assertNeededImports(
    """scala.reflect.BeanProperty""", 
    """
      import scala.reflect.BeanProperty
      case class JavaPerson(@BeanProperty var name: String, @BeanProperty var addresses: java.lang.Object)
      """)    
  @Test
  def classAttributeDeps = assertDependencies(
    """scala.collection.mutable.Map""",
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
  def renamedImport = assertDependencies(
    """scala.collection.mutable.Map""",
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
    """x.B""",
    """
      class A {
        val B = new {
          val y = 2
        }
      }

      object C {
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

      object C {
        def m(x: A) {
          import x._
          println(B.y)
        }
      }
      """)    
    
  @Test
  def classAttributeWithFullPackage = assertDependencies(
    """scala.collection.mutable.Map""",
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
    """java.util
       java.util.ArrayList""",
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
  def importIsUsedAsTypeAscription = assertDependencies(
    """scala.collection.immutable
       scala.collection.immutable.Set
       scala.this.Predef.any2ArrowAssoc""",
    """
      class UsesSet { val s: collection.immutable.Set[Int] = Map(1 -> 2).toSet }
      """)
      
  @Test
  def importIsUsedAsTypeAscriptionNeeded = assertNeededImports(
    "",
    """
      class UsesSet { val s: collection.immutable.Set[Int] = Map(1 -> 2).toSet }
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
      object A {val x: ListBuffer[Int]; val y: ListBuffer[Int]}
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
    """java.ju.Map
       java.util""",
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
    """scala.collection.`package`.breakOut""",
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
  def importFromPackageObjectNeeded = assertNeededImports(
    """scala.collection.`package`.breakOut""",
    """
      import scala.collection.breakOut
      object TestbreakOut {
        val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
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
    """java.util
       java.util.List
       scala.collection.JavaConversions.bufferAsJavaList
       scala.collection.mutable
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
  def importedImplicitConversionNeedsImport2 = assertNeededImports(
    """scala.this.collection.JavaConversions.asScalaBuffer""",
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
    """impl.args.Implicits.x""",
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
}

