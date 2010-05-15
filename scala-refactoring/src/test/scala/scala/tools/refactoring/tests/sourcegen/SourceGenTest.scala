/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.sourcegen

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert
import org.junit.Assert._
import scala.tools.refactoring.sourcegen._
import scala.tools.refactoring.common._
import scala.tools.nsc.ast.Trees
import scala.tools.nsc.io.AbstractFile
import tools.nsc.symtab.Flags
import scala.tools.nsc.ast.parser.Tokens

@Test
class SourceGenTest extends TestHelper with SourceGen with LayoutHelper with Formatting with AstTransformations with ConsoleTracing {
  
  import global._
  import Transformations._
  
  def treeForFile(file: AbstractFile) = {
    unitOfFile get file map (_.body) flatMap removeAuxiliaryTrees
  }
  
  implicit def stringToPrettyPrint(original: String) = new {
    def cleanTree(s: String) = (removeAuxiliaryTrees &> emptyAllPositions)(treeFrom(s)).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)))
  }
  
  implicit def treeToPrettyPrint(original: Tree) = new {
    def cleanTree(t: Tree) = (removeAuxiliaryTrees &> emptyAllPositions)(t).get
    def prettyPrintsTo(expected: String) = assertEquals(expected, generate(cleanTree(original)))
  }
  
  val reverseBody = Transformations.transform[Tree, Tree] {
    case t: Template => t.copy(body = t.body.reverse) setPos t.pos
  }
  
  val doubleAllDefNames = Transformations.transform[Tree, Tree] {
    case t: DefDef => t.copy(name = t.name.toString + t.name.toString) setPos t.pos
  }
  
  val negateAllBools = Transformations.transform[Tree, Tree] {
    case l @ Literal(Constant(true )) => Literal(Constant(false)) setPos l.pos
    case l @ Literal(Constant(false)) => Literal(Constant(true )) setPos l.pos
  }
  
  val changeSomeModifiers = Transformations.transform[Tree, Tree] {
    case t: ClassDef =>
      t.copy(mods = NoMods) setPos t.pos
    case t: DefDef   =>
      t.copy(mods = NoMods withPosition (Flags.PROTECTED, NoPosition) withPosition (Flags.METHOD, NoPosition)) setPos t.pos
    case t: ValDef   =>
      t.copy(mods = NoMods withPosition (Tokens.VAL,   NoPosition)) setPos t.pos
    case t => t 
  }
  
  @Test
  def testNew() = {
    
    val tree = treeFrom("""
    object Functions {
      val a = new String("hello")
      def createNew = new {
        println("hello from an anonymous class")
      }
    }
    """)
        
    assertEquals("""
    object Functions {
      val a = new String("hello")
      def createNew = new {
        println("hello from an anonymous class")
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get))     
    
    // terrible, but don't know how to do better :/
    tree prettyPrintsTo """object Functions {
  val a = new String("hello")
  def createNew = {
    class $anon {
      println("hello from an anonymous class")
    }
    
    new 
  }
}
"""
  }
  
  @Test
  def testReturn() = {
    val tree = treeFrom("""
    object Functions {
      def test {
        return 5
      }
    }
    """)
        
    assertEquals("""
    object Functions {
      def test {
        return 5
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get))     
    
    tree prettyPrintsTo """object Functions {
  def test = return {
    5
  }
}
"""
  }
  
  @Test
  def testPackages() = {
    val tree = treeFrom("""
    package a
    package b.c
    package d {
      package e.f {
      }
    }
    object A
    """)
        
    assertEquals("""
    package a
    package b.c
    package d {
      package e.f {
      }
    }
    object A
    """, generate(removeAuxiliaryTrees apply tree get))     
    
    // XXX this is wrong
    tree prettyPrintsTo """package a
package b.c
package d
package e.f

object A"""
  }
  
  @Test
  def testIfs() = {
    val tree = treeFrom("""
    object Functions {

      val x = if(true) false else true

      val y = if(true == false)
          true
        else if (true == true)
          false
        else
          true

      val z = if(true == false) {
          true
        } else if (true == true) {
          false
        } else {
          println("hello!")
          true
        }
    }""")
    
    assertEquals("""
    object Functions {

      val x = if(false) true else false

      val y = if(false == true)
          false
        else if (false == false)
          true
        else
          false

      val z = if(false == true) {
          false
        } else if (false == false) {
          true
        } else {
          println("hello!")
          false
        }
    }""", generate(removeAuxiliaryTrees &> ↓(any(negateAllBools)) apply tree get))     
    
    tree prettyPrintsTo """object Functions {
  val x = if (true) false else true
  val y = if (true.==(false)) true else if (true.==(true)) false else true
  val z = if (true.==(false)) true else if (true.==(true)) false else {
    println("hello!")
    true
  }
}
"""
  }
    
  @Test
  def testFunctions() = {
    
    val tree = treeFrom("""
    object Functions {
      List(1, 2) map ((i: Int) => i + 1)
      val sum: Seq[Int] => Int = _ reduceLeft (_+_)
      List(1, 2) map (_ + 1)
      List(1, 2) map (i => i + 1)
    }""")
      
    assertEquals("""
    object Functions {
      List(1, 2) map ((i: Int) => i + 1)
      val sum: Seq[Int] => Int = _ reduceLeft (_+_)
      List(1, 2) map (_ + 1)
      List(1, 2) map (i => i + 1)
    }""", generate(removeAuxiliaryTrees apply tree get))     
      
    tree prettyPrintsTo """object Functions {
  List(1, 2).map((i: Int) => i.+(1))
  val sum: (Seq[Int]) => Int = _.reduceLeft(_.+(_))
  List(1, 2).map(_.+(1))
  List(1, 2).map((i) => i.+(1))
}
"""
  }
  
  @Test
  def testTypeDefs() = {
    
    val tree = treeFrom("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""")
    
    assertEquals("""
    trait Types {
      type A = Int
      type B >: Nothing <: AnyRef
      def id[C](c: C) = c
      protected type C >: Nothing
      type D <: AnyRef
    }""", generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """trait Types {
  type A = Int
  type B >: Nothing <: AnyRef
  def id[C](c: C) = c
  protected type C >: Nothing 
  type D <: AnyRef
}
"""
  }
  
  @Test
  def testTypes() = {
    
    val tree = treeFrom("""
    object Rename1 {
      case class Person(name: String)
      def printName(ppp: Person) = println(ppp.name)
      def main(args: Array[String]) {
        val people: List[Person] = List(Person("Mirko"), Person("Christina"))
        people foreach printName
      }
    }""")
    
    assertEquals("""
    object Rename1 {
      case class Person(name: String)
      def printName(ppp: Person) = println(ppp.name)
      def main(args: Array[String]) {
        val people: List[Person] = List(Person("Mirko"), Person("Christina"))
        people foreach printName
      }
    }""", generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """object Rename1 {
  case class Person(name: String)
  def printName(ppp: Rename1.Person) = println(ppp.name)
  def main(args: Array[String]) = {
    val people: List[Rename1.Person] = List(Person("Mirko"), Person("Christina"))
    people.foreach((ppp: Rename1.Person) => printName(ppp))
  }
}
"""
  }
  
  @Test
  def testObjectTemplate() = {
    
    val tree = treeFrom("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """)
    
    assertEquals("""
    object Obj extends java.lang.Object {
      val self = this
    }
    """, generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """object Obj extends java.lang.Object {
  val self = this
}
"""
  }
  
  @Test
  def testValOrDefDefModifiers() = {
    
    val tree = treeFrom("""
    class A {
      /*a*/private/*b*/def/*c*/test() = 5
      lazy val i = 5
      final protected def a() = i
    }
    """)
    
    assertEquals("""
    class A {
      /*a*/protected def test() = 5
      val i = 5
      protected def a() = i
    }
    """, generate((removeAuxiliaryTrees &> ↓(changeSomeModifiers)) apply tree get))
    
    tree prettyPrintsTo """class A {
  private def test = 5
  lazy val i = 5
  final protected def a = i
}
"""
  }
  
  @Test
  def testClassModifiers() = {
    
    val tree = treeFrom("""
    package xy
    abstract class A
    sealed class B
    final class C
    protected sealed class D
    """)
    
    val modTree = (removeAuxiliaryTrees &> ↓(changeSomeModifiers)) apply tree get
    
    assertEquals("""
    package xy
    class A
    class B
    class C
    class D
    """, generate(modTree))
    
    tree prettyPrintsTo """package xy
abstract class A
sealed class B
final class C
protected sealed class D"""
    
    modTree prettyPrintsTo """package xy
class A
class B
class C
class D"""
  }
  
  @Test
  def testSelfTypes() = {

    val tree = treeFrom("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    }
    """)
    
    assertEquals("""
    trait ATrait {
      self =>
    }
    trait BTrait {
      self: ATrait =>
    }
    trait CTrait {
      self: BTrait with ATrait =>
    }
    """, generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """trait ATrait {
  self =>
}
trait BTrait {
  self: ATrait =>
}
trait CTrait {
  self: BTrait with ATrait =>
}"""
  }
  
  @Test
  def testClassTemplates() = {
    
    val tree = treeFrom("""
    trait ATrait
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """)
    
    assertEquals("""
    trait ATrait
    class ASuperClass(x: Int, val d: String)
    class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
      self_type_annotation =>
      def someMethod() {
      }
    }
    """, generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """trait ATrait
class ASuperClass(x: Int, val d: String)
class AClass(i: Int, var b: String, val c: List[String]) extends ASuperClass(i, b) with ATrait {
  self_type_annotation =>
  def someMethod {
  }
}"""
  }
  
  @Test
  def testEarlyDef() = {
    val tree = treeFrom("""
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
""")

    assertEquals("""
    trait Greeting {
      val name: String
      val msg = "How are you, "+name
    }
    class C(i: Int) extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
""", generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """trait Greeting {
  val name: String
  val msg = "How are you, ".+(name)
}

class C(i: Int) extends {
  val name = "Bob"
} with Greeting {
  println(msg)
}
"""
  }
  
  @Test
  def testImports() = {
    val tree = treeFrom("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """)

    assertEquals("""
    import java.lang.{String => S}
    import java.lang.Object
    import java.lang.{String => S, Object => _, _}
    import scala.collection.mutable._
    """, generate(removeAuxiliaryTrees apply tree get))
    
    tree prettyPrintsTo """import java.lang.{String => S}
import java.lang.Object
import java.lang.{String => S, Object => _, _}
import scala.collection.mutable._"""
  }
  
  @Test
  def testMethods() = {
    
    val tree = treeFrom("""
      trait ATest
      {
        def abcd[T](a: String, b: Int): Int
        def a() = 5
        def b = 42
        def ab(i: Int)(j: Int) = (i ,     j)
        def timesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def square {
          def nested(i: Int) = {
            i * i
          }
          nested(5)
        }
      }
    """)

    assertEquals("""
      trait ATest
      {
        def squaresquare {
          def nestednested(i: Int) = {
            i * i
          }
          nested(5)
        }
        def timesTwotimesTwo(i: Int) = {
          i.*(5)
          i * 2
        }
        def abab(i: Int)(j: Int) = (i ,     j)
        def bb = 42
        def aa() = 5
        def abcdabcd[T](a: String, b: Int): Int
      }
    """, generate(removeAuxiliaryTrees &> ↓(⊆(doubleAllDefNames)) &> ↓(⊆(reverseBody)) apply tree get)) 
  }
  
  @Test
  def testVals() = {
    val tree = treeFrom("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val a: Int = 5 //a
        val b = "huhu" //b
      }
      trait B
      {
        val a: Int
      }
    }
    """)

    assertEquals("""
    /*a*/package /*b*/xyz/*c*/ {
      // now a class
      trait A
      {
        val b = "huhu" //b
        val a: Int = 5 //a
      }
      trait B
      {
        val a: Int
      }
    }
    """, generate(removeAuxiliaryTrees &> ↓(⊆(reverseBody)) apply tree get))
    
    tree prettyPrintsTo """package xyz
trait A {
  val a: Int = 5
  val b = "huhu"
}

trait B {
  val a: Int
}
"""
  }
  
  @Test
  def testMethodSignatures() = """
    package xy
    
    class A {
      def a(): Int
      def b(): Int = 5
      def c() = 5
      def d() = {
        val a = 5
        a
      }
      def e(i: Int) = i
      def f(i: Int)(j: Int) = i+j
      def g(i: Int, j: Int) = i+j
      def h(i: Int, j: Int): (Int, Int) = (i, j)
      def id[A](a: A) = a
    }
    """ prettyPrintsTo """package xy
class A {
  def a: Int
  def b: Int = 5
  def c = 5
  def d = {
    val a = 5
    a
  }
  def e(i: Int) = i
  def f(i: Int)(j: Int) = i.+(j)
  def g(i: Int, j: Int) = i.+(j)
  def h(i: Int, j: Int): (Int, Int) = (i, j)
  def id[A](a: A) = a
}
"""
}

