/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.Rename
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._

class RenameTest extends TestHelper with TestRefactoring {
  outer =>
  
  def renameTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new Rename with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes) 
    }
    val changes = performRefactoring(name)
  }.changes
  
  @Test
  def renameRecursive = new FileSet {
    """
      package renameRecursive
      class A {
        def length[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/  length(xs)  /*)*/
        }
      }
    """ becomes
    """
      package renameRecursive
      class A {
        def recursiveLength[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/  recursiveLength(xs)  /*)*/
        }
      }
    """
  } applyRefactoring(renameTo("recursiveLength")) 
  
  @Test
  def renameLocalValue = new FileSet {
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
    """ becomes
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
    """
  } applyRefactoring(renameTo("b"))
        
  @Test
  def renameLazyVal = new FileSet {
    """
    package renameLazyVal
    class A {
      def rename() {
        lazy val a = 5
        println(/*(*/a/*)*/)
      }
    }
    """ becomes
    """
    package renameLazyVal
    class A {
      def rename() {
        lazy val b = 5
        println(/*(*/b/*)*/)
      }
    }
    """
  } applyRefactoring(renameTo("b"))
        
  @Test
  def renameParameter = new FileSet {
    """
    package renameParameter
    class A {
      def rename(  /*(*/a/*)*/  : String) {
        println(a)
      }
    }
    """ becomes
    """
    package renameParameter
    class A {
      def rename(  /*(*/b/*)*/  : String) {
        println(b)
      }
    }
    """
  } applyRefactoring(renameTo("b"))
    
  @Test
  def renameWithType = new FileSet {
    """
    package renameWithType
    class A {
      def rename(a: String) {
        a match {
          case b: String => /*(*/  b  /*)*/
        }
      }
    }
    """ becomes
    """
    package renameWithType
    class A {
      def rename(a: String) {
        a match {
          case c: String => /*(*/  c  /*)*/
        }
      }
    }
    """
  } applyRefactoring(renameTo("c"))
    
  @Test
  def renameMultiAssignment = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/a/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/c/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))
    
  @Test
  def renameDeconstructingAssignment = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print {
        val List(/*(*/a/*)*/, b) = List(5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print {
        val List(/*(*/c/*)*/, b) = List(5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))
    
  @Test
  def renameBinding = new FileSet {
    """
    package renameBinding
    class A {
      def print {
        1 match { case /*(*/ i /*)*/ => i }
      }
    }
    """ becomes
    """
    package renameBinding
    class A {
      def print {
        1 match { case /*(*/ integer /*)*/ => integer }
      }
    }
    """
  } applyRefactoring(renameTo("integer"))
    
  @Test
  def renameNewVal = new FileSet {
    """
    package renameNewVal
    class A(i: Int) {
      def print {
        var  /*(*/  l = /*)*/  new A(5)
      }
    }
    """ becomes
    """
    package renameNewVal
    class A(i: Int) {
      def print {
        var  /*(*/  ls = /*)*/  new A(5)
      }
    }
    """
  } applyRefactoring(renameTo("ls"))
    
  @Test
  def renameLazyArg = new FileSet {
    """
    package renameLazyArg
    class A(i: Int) {
      def print(a: => String) {
        println(/*(*/  a  /*)*/)
      }
    }
    """ becomes
    """
    package renameLazyArg
    class A(i: Int) {
      def print(s: => String) {
        println(/*(*/  s  /*)*/)
      }
    }
    """
  } applyRefactoring(renameTo("s"))
        
  @Test
  def forComprehension = new FileSet {
    """
    package forComprehension
    class A {
      def print {
        for(  /*(*/  i  /*)*/  <- 1 to 10) yield i
      }
    }
    """ becomes
    """
    package forComprehension
    class A {
      def print {
        for(  /*(*/  index  /*)*/  <- 1 to 10) yield index
      }
    }
    """
  } applyRefactoring(renameTo("index"))
            
  @Test
  def inConstructor = new FileSet {
    """
    package inConstructor
    class A(i: Int) {
      val /*(*/  y  /*)*/ = i * 2
    }
    """ becomes
    """
    package inConstructor
    class A(i: Int) {
      val /*(*/  iTimesTwo  /*)*/ = i * 2
    }
    """
  } applyRefactoring(renameTo("iTimesTwo"))
            
  @Test
  def renameMethod = new FileSet {
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
    """ becomes
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
    """
  } applyRefactoring(renameTo("x"))
    
  @Test
  def renameMethodInMultipleFiles = new FileSet {
    """
    package rename
    class A {
      /*(*/  def get(a: Int) = "get"  /*)*/
    }
    """ becomes
    """
    package rename
    class A {
      /*(*/  def x(a: Int) = "get"  /*)*/
    }
    """
    ;
    """
    package rename
    class B {
      val a = new A
      val get = a.get(5)
    }
    """ becomes
    """
    package rename
    class B {
      val a = new A
      val get = a.x(5)
    }
    """   
  } applyRefactoring(renameTo("x"))
    
  @Test
  def renameClass = new FileSet {
    """
    package renameClass
    /*(*/  class A  /*)*/

    class B extends A
    """ becomes
    """
    package renameClass
    /*(*/  class X  /*)*/

    class B extends X
    """
    ;
    """
    package renameClass
    object C extends A {
      val a = new A
      val as = new collection.mutable.ListBuffer[A]
      def doWithA(a: A) = new A
    }
    """ becomes
    """
    package renameClass
    object C extends X {
      val a = new X
      val as = new collection.mutable.ListBuffer[X]
      def doWithA(a: X) = new X
    }
    """   
  } applyRefactoring(renameTo("X"))
    
  @Test
  def renameTypeParameter = new FileSet {
    """
    package ex

    trait Monad[ /*(*/M/*)*/[_]] {
      def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
      def unital[A](a: A): M[A]
    }
    """ becomes
    """
    package ex

    trait Monad[ /*(*/Md/*)*/[_]] {
      def flatMap[A, B](a: Md[A], f: A => Md[B]): Md[B]
      def unital[A](a: A): Md[A]
    }
    """
  } applyRefactoring(renameTo("Md"))
    
  @Test
  def renameSuperClass = new FileSet {
    """
    package ex

    sealed abstract class /*(*/Term/*)*/
    
    case object TmTrue extends Term
    case class  TmIf(t1: Term, t2: Term, t3: Term) extends Term
    """ becomes
    """
    package ex

    sealed abstract class /*(*/Expr/*)*/
    
    case object TmTrue extends Expr
    case class  TmIf(t1: Expr, t2: Expr, t3: Expr) extends Expr
    """
  } applyRefactoring(renameTo("Expr"))
    
  @Test
  def renameType = new FileSet {
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/Person/*)*/(name: String)
      
      def main(args: Array[String]) {
        
        val people: List[Person] = List(new Person("Mirko"), new Person("Christina"))
        
      }
    }""" becomes
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/P/*)*/(name: String)
      
      def main(args: Array[String]) {
        
        val people: List[P] = List(new P("Mirko"), new P("Christina"))
        
      }
    }"""
  } applyRefactoring(renameTo("P"))
    
  @Test
  def renameCaseClass = new FileSet {
    """
    case class /*(*/Person/*)*/(name: String)

    object Rename1 {
      val p = Person("Mirko")
    }""" becomes
    """
    case class /*(*/P/*)*/(name: String)

    object Rename1 {
      val p = P("Mirko")
    }"""
  } applyRefactoring(renameTo("P"))
    
  //@Test FIXME renameByNameParameters
  def renameByNameParameters = new FileSet {
    """
    object ByNameParap {
      def withParam(param1: Int,/*(*/name/*)*/: String) = println(name)
      withParam(name = "Mirko", param1 = 5)
      withParam(5, "Mirko")
    }""" becomes
    """"""
  } applyRefactoring(renameTo("n"))
    
  @Test
  def renameSelfType = new FileSet {
    """
    trait /*(*/T1/*)*/

    trait T3 {
      self: T1 =>

    }""" becomes
    """
    trait /*(*/Trait/*)*/

    trait T3 {
      self: Trait =>

    }"""
  } applyRefactoring renameTo("Trait")
    
  @Test
  def renameSelfType2 = new FileSet {
    """
    trait /*(*/T1/*)*/
    trait T2

    trait T3 {
      self: T1 with T2 =>

    }""" becomes
    """
    trait /*(*/Trait/*)*/
    trait T2

    trait T3 {
      self: Trait with T2 =>

    }"""
  } applyRefactoring(renameTo("Trait"))
    
  @Test
  def renameClassWithImport = new FileSet {
    """
    package withTrait
    trait /*(*/T/*)*/
    """ becomes
    """
    package withTrait
    trait /*(*/Trait/*)*/
    """
    ;
    """
    package withoutTrait
    import withTrait.T
    trait TSub extends T
    """ becomes
    """
    package withoutTrait
    import withTrait.Trait
    trait TSub extends Trait
    """
  } applyRefactoring(renameTo("Trait"))
    
  @Test
  def renamePackages = new FileSet {
    """
    package /*(*/p1/*)*/
    """ becomes
    """
    package /*(*/refactoring/*)*/
    """
    ;
    """
    package p1.p2
    import  p1._
    """ becomes
    """
    package refactoring.p2
    import  refactoring._
    """
  } applyRefactoring(renameTo("refactoring"))
    
  @Test
  def renameInnerPackages = new FileSet {
    """
    package p1
    package /*(*/   p2  /*)*/ {
      package p3 {
        class A
      }
    }
    """ becomes
    """
    package p1
    package /*(*/   refactoring  /*)*/ {
      package p3 {
        class A
      }
    }
    """
    ;
    """
    package p1
    class B extends  p2.p3.A
    """ becomes
    """
    package p1
    class B extends  refactoring.p3.A
    """
  } applyRefactoring(renameTo("refactoring"))
    
  @Test
  def renameUnderWindows = new FileSet {
    "package com.test\r\nobject Hello {\r\n  def test = {\r\n    val /*(*/loc/*)*/ = 42\r\n    loc * loc\r\n  }\r\n}" becomes
    "package com.test\r\nobject Hello {\r\n  def test = {\r\n    val /*(*/fourtytwo/*)*/ = 42\r\n    fourtytwo * fourtytwo\r\n  }\r\n}"
  } applyRefactoring(renameTo("fourtytwo"))
  
      
  @Test
  def renameCaseClassAndObject = new FileSet {
    """
      object /*(*/TestIde/*)*/ {}

      case class TestIde {}
    """ becomes
    """
      object /*(*/TestIde1/*)*/ {}

      case class TestIde1 {}
    """
  } applyRefactoring(renameTo("TestIde1"))
  
  @Test
  def renameNestedType = new FileSet {
    """
    trait /*(*/Thing/*)*/ {
      val otherThings: Set[Thing] = Set()
      val myThing: Thing
    }
    """ becomes
    """
    trait /*(*/X/*)*/ {
      val otherThings: Set[X] = Set()
      val myThing: X
    }
    """
  } applyRefactoring(renameTo("X"))
  
  @Test
  def renameIdentifierWithBackticks = new FileSet {
    """
    trait StrangeIdentifiers {
      val /*(*/`my strange identifier`/*)*/ = "foo"
      val `my strange identifier 2` = `my strange identifier`
    }
    """ becomes
    """
    trait StrangeIdentifiers {
      val /*(*/`my strange identifier again`/*)*/ = "foo"
      val `my strange identifier 2` = `my strange identifier again`
    }
    """
  } applyRefactoring(renameTo("my strange identifier again"))
}
