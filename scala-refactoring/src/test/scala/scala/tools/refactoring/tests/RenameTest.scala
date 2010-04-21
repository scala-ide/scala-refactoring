/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests

import scala.tools.refactoring.implementations.Rename
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.ConsoleTracing
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class RenameTest extends TestHelper with TestRefactoring {
  outer =>
  
  def renameTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new Rename with ConsoleTracing {
      val global = outer.global
      pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) foreach ( index processTree _ )
      println(index.debugString)
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters {
      val newName = name
    })
  }.changes
  
  @Test
  def renameRecursive = new FileSet {
    add(
    """
      package renameRecursive
      class A {
        def length[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/  length(xs)  /*)*/
        }
      }
    """,
    """
      package renameRecursive
      class A {
        def recursiveLength[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/  recursiveLength(xs)  /*)*/
        }
      }
    """)
  } applyRefactoring(renameTo("recursiveLength")) 
  
  @Test
  def renameLocalValue = new FileSet {
    add(
    """
      package renameLocal1
      class A {
        def double(s: String) = s + s
        def extractFrom {
          val s = "hallo"
  /*(*/   s   /*)*/ .length   
          double(s + "a")
        }
      }
    """,
    """
      package renameLocal1
      class A {
        def double(s: String) = s + s
        def extractFrom {
          val b = "hallo"
  /*(*/   b   /*)*/ .length   
          double(b + "a")
        }
      }
    """)
  } applyRefactoring(renameTo("b"))
        
  @Test
  def renameParameter = new FileSet {
    add(
    """
    package renameParameter
    class A {
      def rename(  /*(*/a/*)*/  : String) {
        println(a)
      }
    }
    """,
    """
    package renameParameter
    class A {
      def rename(  /*(*/b/*)*/  : String) {
        println(b)
      }
    }
    """)
  } applyRefactoring(renameTo("b"))
    
  @Test
  def renameWithType = new FileSet {
    add(
    """
    package renameWithType
    class A {
      def rename(a: String) {
        a match {
          case b: String => /*(*/  b  /*)*/
        }
      }
    }
    """,
    """
    package renameWithType
    class A {
      def rename(a: String) {
        a match {
          case c: String => /*(*/  c  /*)*/
        }
      }
    }
    """)
  } applyRefactoring(renameTo("c"))
    
  @Test
  def renameMultiAssignment = new FileSet {
    add(
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/a/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """,
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/c/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """)
  } applyRefactoring(renameTo("c"))
    
  @Test
  def renameBinding = new FileSet {
    add(
    """
    package renameBinding
    class A {
      def print {
        1 match { case /*(*/ i /*)*/ => i }
      }
    }
    """,
    """
    package renameBinding
    class A {
      def print {
        1 match { case /*(*/ integer /*)*/ => integer }
      }
    }
    """)
  } applyRefactoring(renameTo("integer"))
    
  @Test
  def renameNewVal = new FileSet {
    add(
    """
    package renameNewVal
    class A(i: Int) {
      def print {
        var  /*(*/  l = /*)*/  new A(5)
      }
    }
    """,
    """
    package renameNewVal
    class A(i: Int) {
      def print {
        var  /*(*/  ls = /*)*/  new A(5)
      }
    }
    """)
  } applyRefactoring(renameTo("ls"))
    
  @Test
  def renameLazyArg = new FileSet {
    add(
    """
    package renameLazyArg
    class A(i: Int) {
      def print(a: => String) {
        println(/*(*/  a  /*)*/)
      }
    }
    """,
    """
    package renameLazyArg
    class A(i: Int) {
      def print(s: => String) {
        println(/*(*/  s  /*)*/)
      }
    }
    """)
  } applyRefactoring(renameTo("s"))
        
  @Test
  def forComprehension = new FileSet {
    add(
    """
    package forComprehension
    class A {
      def print {
        for(  /*(*/  i  /*)*/  <- 1 to 10) yield i
      }
    }
    """,
    """
    package forComprehension
    class A {
      def print {
        for(  /*(*/  index  /*)*/  <- 1 to 10) yield index
      }
    }
    """)
  } applyRefactoring(renameTo("index"))
            
  @Test
  def inConstructor = new FileSet {
    add(
    """
    package inConstructor
    class A(i: Int) {
      val /*(*/  y  /*)*/ = i * 2
    }
    """,
    """
    package inConstructor
    class A(i: Int) {
      val /*(*/  iTimesTwo  /*)*/ = i * 2
    }
    """)
  } applyRefactoring(renameTo("iTimesTwo"))
            
  @Test
  def renameMethod = new FileSet {
    add(
    """
    package renameMethod
    class A {
      def get(a: Int) = "get"
      def main = {
        val a = /*(*/  get  /*)*/  (5)
      }
    }

    class B(a: A) {
      val x = a.get(10)
    }
    """,
    """
    package renameMethod
    class A {
      def x(a: Int) = "get"
      def main = {
        val a = /*(*/  x  /*)*/  (5)
      }
    }

    class B(a: A) {
      val x = a.x(10)
    }
    """)
  } applyRefactoring(renameTo("x"))
    
  @Test
  def renameMethodInMultipleFiles = new FileSet {
    
    add(
    """
    package rename
    class A {
      /*(*/  def get(a: Int) = "get"  /*)*/
    }
    """,
    """
    package rename
    class A {
      /*(*/  def x(a: Int) = "get"  /*)*/
    }
    """)
    
    add(
    """
    package rename
    class B {
      val a = new A
      val get = a.get(5)
    }
    """,
    """
    package rename
    class B {
      val a = new A
      val get = a.x(5)
    }
    """)   
  } applyRefactoring(renameTo("x"))
    
  @Test
  def renameClass = new FileSet {
    
    add(
    """
    package renameClass
    /*(*/  class A  /*)*/

    class B extends A
    """,
    """
    package renameClass
    /*(*/  class X  /*)*/

    class B extends X
    """)
    
    add(
    """
    package renameClass
    object C extends A {
      val a = new A
      val as = new collection.mutable.ListBuffer[A]
      def doWithA(a: A) = new A
    }
    """,
    """
    package renameClass
    object C extends X {
      val a = new X
      val as = new collection.mutable.ListBuffer[X]
      def doWithA(a: X) = new X
    }
    """)   
  } applyRefactoring(renameTo("X"))
    
  @Test
  def renameTypeParameter = new FileSet {
    
    add(
    """
    package ex

    trait Monad[ /*(*/M/*)*/[_]] {
      def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
      def unital[A](a: A): M[A]
    }
    """,
    """
    package ex

    trait Monad[ /*(*/Md/*)*/[_]] {
      def flatMap[A, B](a: Md[A], f: A => Md[B]): Md[B]
      def unital[A](a: A): Md[A]
    }
    """)
  } applyRefactoring(renameTo("Md"))
    
  @Test
  def renameSuperClass = new FileSet {
    
    add(
    """
    package ex

    sealed abstract class /*(*/Term/*)*/
    
    case object TmTrue extends Term
    case class  TmIf(t1: Term, t2: Term, t3: Term) extends Term
    """,
    """
    package ex

    sealed abstract class /*(*/Expr/*)*/
    
    case object TmTrue extends Expr
    case class  TmIf(t1: Expr, t2: Expr, t3: Expr) extends Expr
    """)
  } applyRefactoring(renameTo("Expr"))
    
  @Test
  def renameType = new FileSet {
    
    add(
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/Person/*)*/(name: String)
      
      def main(args: Array[String]) {
        
        val people: List[Person] = List(new Person("Mirko"), new Person("Christina"))
        
      }
    }""",
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/P/*)*/(name: String)
      
      def main(args: Array[String]) {
        
        val people: List[P] = List(new P("Mirko"), new P("Christina"))
        
      }
    }""")
  } applyRefactoring(renameTo("P"))
    
  @Test
  def renameCaseClass = new FileSet {
    
    add(
    """
    case class /*(*/Person/*)*/(name: String)

    object Rename1 {
      val p = Person("Mirko")
    }""",
    """
    case class /*(*/P/*)*/(name: String)

    object Rename1 {
      val p = P("Mirko")
    }""")
  } applyRefactoring(renameTo("P"))
    
  //@Test FIXME renameByNameParameters
  def renameByNameParameters = new FileSet {
    
    add(
    """
    object ByNameParap {
      def withParam(param1: Int,/*(*/name/*)*/: String) = println(name)
      withParam(name = "Mirko", param1 = 5)
      withParam(5, "Mirko")
    }""",
    """""")
  } applyRefactoring(renameTo("n"))
    
  @Test
  def renameSelfType = new FileSet {
    add(
    """
    trait /*(*/T1/*)*/

    trait T3 {
      self: T1 =>

    }""",
    """
    trait /*(*/Trait/*)*/

    trait T3 {
      self: Trait =>

    }""")
  } applyRefactoring(renameTo("Trait"))
}
