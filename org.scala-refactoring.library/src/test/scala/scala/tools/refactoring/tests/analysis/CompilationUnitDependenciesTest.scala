/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.analysis

import tests.util.TestHelper
import org.junit.Assert._
import analysis._

class CompilationUnitDependenciesTest extends TestHelper with CompilationUnitDependencies {

  import global._
  
  def assertNeededImports(expected: String, src: String) {
    val tree = treeFrom(src)
    val imports = neededImports(tree).sortBy(_.toString)
    assertEquals(expected.split("\n").map(_.trim).mkString("\n"), imports.mkString("\n"))
  }

  def assertDependencies(expected: String, src: String) {
    val tree = treeFrom(src)
    val imports = dependencies(tree).sortBy(_.toString)
    assertEquals(expected.split("\n").map(_.trim).mkString("\n"), imports.mkString("\n"))
  }

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
  def classAttributeDeps = assertDependencies(
    """scala.collection.mutable.Map
       scala.collection.mutable.Map.apply""",
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
    """scala.collection.mutable.M.apply
       scala.collection.mutable.Map""",
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
  def classAttributeWithFullPackage = assertDependencies(
    """scala.collection.mutable.Map
         scala.collection.mutable.Map.apply""",
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
  def classAttributeWithWildcardImport = assertDependencies(
    """scala.collection.mutable.HashSet
         scala.collection.mutable.HashSet.apply""",
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
  def importIsUsedAsTypeAscription = assertDependencies(
    """scala.collection.immutable.Set""",
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
    """scala.util.Random""",
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
  def importFromPackageObject = assertDependencies(
    """scala.collection.`package`.breakOut""",
    """
      import scala.collection.breakOut
      object TestbreakOut {
        val xs: Map[Int, Int] = List((1, 1), (2, 2)).map(identity)(breakOut)
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
}

