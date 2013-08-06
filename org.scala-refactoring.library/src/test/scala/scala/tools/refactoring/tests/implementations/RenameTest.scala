/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.Rename
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._
import org.junit.Ignore

import language.reflectiveCalls

class RenameTest extends TestHelper with TestRefactoring {
  outer =>

  def renameTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new Rename with SilentTracing with TestProjectIndex
    val changes = performRefactoring(name)
  }.changes

  @Test
  def renameOverlapping = new FileSet {
    """
      package renameOverlapping

      case class /*(*/Die/*)*/(top: Int, right: Int, front: Int) {
        import Transform._

        def transform(how: Boolean) = how match {
          case true => Die(front, right, top)
          case false => Die(front, right, top)
        }
      }
      object Transform
    """ becomes
    """
      package renameOverlapping

      case class /*(*/Dice/*)*/(top: Int, right: Int, front: Int) {
        import Transform._

        def transform(how: Boolean) = how match {
          case true => Dice(front, right, top)
          case false => Dice(front, right, top)
        }
      }
      object Transform
    """
  } applyRefactoring(renameTo("Dice"))

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
  def renameLazyValFromDefinition = new FileSet {
    """
    package renameLazyVals
    class A {
      def print {
        lazy val /*(*/a = 42/*)*/
        println(a)
      }
    }
    """ becomes
    """
    package renameLazyVals
    class A {
      def print {
        lazy val /*(*/c = 42/*)*/
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

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
  def renameMultiAssignmentWithTA = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/a: Int/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print {
        val (/*(*/c: Int/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameClassWithTypeParameters = new FileSet {
    """
    case class /*(*/Test/*)*/[A, B](a:A,b:B)
    """ becomes
    """
    case class /*(*/Test1/*)*/[A, B](a:A,b:B)
    """
  } applyRefactoring(renameTo("Test1"))

  @Test
  def renameAbstractType = new FileSet {
    """
    trait O {
      trait Property[+T]
      type /*(*/Prop_Tp/*)*/[+Vl_Tpe] <: Property[Vl_Tpe]
      def properties: Set[Prop_Tp[_]] = null.asInstanceOf[Set[Prop_Tp[_]]]
    }
    """ becomes
    """
    trait O {
      trait Property[+T]
      type /*(*/Prop_Tpe/*)*/[+Vl_Tpe] <: Property[Vl_Tpe]
      def properties: Set[Prop_Tpe[_]] = null.asInstanceOf[Set[Prop_Tpe[_]]]
    }
    """
  } applyRefactoring(renameTo("Prop_Tpe"))

  @Test
  def renameReferenceToOuterclass = new FileSet {
    """
    package renameReferenceToOuterclass
    class /*(*/Foo/*)*/ {
      class Bar {
        def foo = Foo.this
      }
    }
    """ becomes
    """
    package renameReferenceToOuterclass
    class /*(*/Blubb/*)*/ {
      class Bar {
        def foo = Blubb.this
      }
    }
    """
  } applyRefactoring(renameTo("Blubb"))

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
  } applyRefactoring(renameTo("`my strange identifier again`"))

  @Test
  def renameClassWithBackTicks = new FileSet {
    """
    package renameClassWithBackTicks
    /*(*/
    class `A` {
      val a = new `A`
    } /*)*/
    """ becomes
    """
    package renameClassWithBackTicks
    /*(*/
    class `X Y` {
      val a = new `X Y`
    } /*)*/
    """
  } applyRefactoring(renameTo("`X Y`"))

  @Test
  def renameTypeParamInSecondConstructor = new FileSet {
    """
    trait /*(*/X/*)*/

    class StrangeIdentifiers () {
      def this(comparator: java.util.Comparator[X]) = {
        this()
      }
    }
    """ becomes
    """
    trait /*(*/Comp/*)*/

    class StrangeIdentifiers () {
      def this(comparator: java.util.Comparator[Comp]) = {
        this()
      }
    }
    """
  } applyRefactoring(renameTo("Comp"))

  @Test
  def renameSingleVariableDeconstructingAssignment = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment
    class A {
      def print {
        val List(/*(*/a/*)*/, _) = List(5, 6)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment
    class A {
      def print {
        val List(/*(*/c/*)*/, _) = List(5, 6)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment2 = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print {
        val Some(/*(*/a/*)*/) = Some(6)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print {
        val Some(/*(*/c/*)*/) = Some(6)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment3 = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print {
        val Reg = "(\\w)".r
        val Reg(/*(*/a/*)*/) = "x"
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print {
        val Reg = "(\\w)".r
        val Reg(/*(*/c/*)*/) = "x"
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment4 = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment4
    class A {
      def print {
        val List(_, /*(*/a/*)*/) = List(1, 2)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment4
    class A {
      def print {
        val List(_, /*(*/c/*)*/) = List(1, 2)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment5 = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment5
    class A {
      def print {
        val List(_, Some(List(_, Some(/*(*/a/*)*/), _))) = List(None, Some(List(1, Some(2), 3)))
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment5
    class A {
      def print {
        val List(_, Some(List(_, Some(/*(*/c/*)*/), _))) = List(None, Some(List(1, Some(2), 3)))
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment6 = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment6
    class A {
      def print {
        val List(/*(*/a/*)*/: Int) = List(42)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment6
    class A {
      def print {
        val List(/*(*/c/*)*/: Int) = List(42)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))


  @Test
  def renameClassParameterPassedIntoSuperClassWithExpression = new FileSet {
    """
    class Class(/*(*/a/*)*/: String) extends RuntimeException(a + "")
    """ becomes
    """
    class Class(/*(*/b/*)*/: String) extends RuntimeException(b + "")
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameClassParameterPassedIntoSuperClassWithExpression2 = new FileSet {
    """
    package renameClassParameterPassedIntoSuperClassWithExpression2
    class Class(val /*(*/a/*)*/: String) extends RuntimeException(a + "")
    """ becomes
    """
    package renameClassParameterPassedIntoSuperClassWithExpression2
    class Class(val /*(*/b/*)*/: String) extends RuntimeException(b + "")
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameSuperclassAtEndOfFile = new FileSet {
    """
    package renameSuperclassAtEndOfFile
    class /*(*/Bar/*)*/
    class Foo extends Bar""" becomes
    """
    package renameSuperclassAtEndOfFile
    class /*(*/Baz/*)*/
    class Foo extends Baz"""
  } applyRefactoring(renameTo("Baz"))

  @Test
  def renameSuperclassAtEndOfFile2 = new FileSet {
    """
    package renameSuperclassAtEndOfFile
    class B
    class /*(*/A/*)*/ extends B""" becomes
    """
    package renameSuperclassAtEndOfFile
    class B
    class /*(*/Bazadudud/*)*/ extends B"""
  } applyRefactoring(renameTo("Bazadudud"))

  @Test
  def renameMethodInCaseObject = new FileSet {
    """
  abstract class Base {
    def /*(*/foo/*)*/ = false
  }

  case object Obj extends Base {
    override def foo = true
  }

  case class Claz extends Base {
    override def foo = true
  } """ becomes
    """
  abstract class Base {
    def /*(*/babar/*)*/ = false
  }

  case object Obj extends Base {
    override def babar = true
  }

  case class Claz extends Base {
    override def babar = true
  } """
  } applyRefactoring(renameTo("babar"))

  @Test
  def renameClassWithClassOfUsage = new FileSet {
    """
    package renameClassWithClassOfUsage
    class /*(*/Foo/*)*/ {
      val clazz = classOf[Foo]
    }
    """ becomes
    """
    package renameClassWithClassOfUsage
    class /*(*/Bar/*)*/ {
      val clazz = classOf[Bar]
    }
    """
  } applyRefactoring(renameTo("Bar"))

  @Test
  def renameClassExplicitSelfTypeAnnotation= new FileSet {
    """
    package renameClassExplicitSelfTypeAnnotation
    trait A
    class /*(*/Foo/*)*/ {
      self: Foo with A=>
    }
    """ becomes
    """
    package renameClassExplicitSelfTypeAnnotation
    trait A
    class /*(*/Babar/*)*/ {
      self: Babar with A=>
    }
    """
  } applyRefactoring(renameTo("Babar"))

  @Test
  def renameWithMultipleContextBounds = new FileSet {
    """
    package test
    class Foo[T] {
      def /*(*/bar/*)*/[A: Numeric: Foo] = ""
    }
    """ becomes
    """
    package test
    class Foo[T] {
      def /*(*/babar/*)*/[A: Numeric: Foo] = ""
    }
    """
  } applyRefactoring(renameTo("babar"))

  @Test
  def renameClassWithThisConstuctorCall = new FileSet {
    """
    package renameClassWithThisConstuctorCall

    class /*(*/Config/*)*/(sourcePaths: Set[String], outputDir: String = null) {
     def this() = this(Set())
    }
    """ becomes
    """
    package renameClassWithThisConstuctorCall

    class /*(*/ConfigX/*)*/(sourcePaths: Set[String], outputDir: String = null) {
     def this() = this(Set())
    }
    """
  } applyRefactoring(renameTo("ConfigX"))

  @Test
  def renameAbstractTypesInHierarchy = new FileSet {
    """
    abstract class A {
      type /*(*/Foo/*)*/
      abstract class B extends A {
        type Foo
        class C extends B {
          type Foo = Unit
        }
      }
    }
    """ becomes
    """
    abstract class A {
      type /*(*/ConfigX/*)*/
      abstract class B extends A {
        type ConfigX
        class C extends B {
          type ConfigX = Unit
        }
      }
    }
    """
  } applyRefactoring(renameTo("ConfigX"))

  @Test
  def renameClassSelfTypeAnnotation = new FileSet {
    """
    package renameClassWithSelfTypeAnnotation
    class /*(*/Foo/*)*/ {
      self =>
    }
    """ becomes
    """
    package renameClassWithSelfTypeAnnotation
    class /*(*/Bar/*)*/ {
      self =>
    }
    """
  } applyRefactoring(renameTo("Bar"))

  @Test
  def renameClassSelfTypeAnnotation2 = new FileSet {
    """
    package renameClassWithSelfTypeAnnotation
    class /*(*/Foo/*)*/ {
      self: Foo =>
    }
    """ becomes
    """
    package renameClassWithSelfTypeAnnotation
    class /*(*/Babar/*)*/ {
      self: Babar =>
    }
    """
  } applyRefactoring(renameTo("Babar"))

  @Test
  def renameMethodWithContextBound = new FileSet {
    """
object RenameWithContextBound {
  val blubb = new Blubb

  def /*(*/work/*)*/[A: Foo](f: Blubb => A): A = f(blubb) ensuring {
    implicitly[Foo[A]].foo(_) >= 42
  }
}

trait Foo[A] {
  def foo(a: A): Int
}

class Blubb
    """ becomes
    """
object RenameWithContextBound {
  val blubb = new Blubb

  def /*(*/abc/*)*/[A: Foo](f: Blubb => A): A = f(blubb) ensuring {
    implicitly[Foo[A]].foo(_) >= 42
  }
}

trait Foo[A] {
  def foo(a: A): Int
}

class Blubb
    """
  } applyRefactoring(renameTo("abc"))

  @Test
  def coloncolon = new FileSet {
    """
    object Problem07 {
      def /*(*/flatten/*)*/(list: List[Any]): List[Any] = list match {
        case (head@_::_)::tail => flatten(head) ++ flatten(tail)
        case Nil::tail => flatten(tail)
        case head::tail => head::flatten(tail)
        case Nil => Nil
      }
    }
    """ becomes
    """
    object Problem07 {
      def /*(*/fltn/*)*/(list: List[Any]): List[Any] = list match {
        case (head@_::_)::tail => fltn(head) ++ fltn(tail)
        case Nil::tail => fltn(tail)
        case head::tail => head::fltn(tail)
        case Nil => Nil
      }
    }
    """
  } applyRefactoring(renameTo("fltn"))

  @Test
  def typeProjection = new FileSet {
    """
    class A {
      trait /*(*/B/*)*/
      def foo(b: A#B) {}
    }
    """ becomes
    """
    class A {
      trait /*(*/C/*)*/
      def foo(b: A#C) {}
    }
    """
  } applyRefactoring(renameTo("C"))

  @Test
  def overriddenMethod = new FileSet {
    """
    package overriddenMethod.bar

    trait Bar {
      def bippy: String

      def bar = bippy.toUpperCase
    }
    """ becomes """
    package overriddenMethod.bar

    trait Bar {
      def booh: String

      def bar = booh.toUpperCase
    }
    """
    ;
    """
    package overriddenMethod
    import bar.Bar

    class Foo extends Bar {
      override def /*(*/bippy/*)*/: String = "foo"
    }
    """ becomes
    """
    package overriddenMethod
    import bar.Bar

    class Foo extends Bar {
      override def /*(*/booh/*)*/: String = "foo"
    }
    """
  } applyRefactoring(renameTo("booh"))

  @Ignore
  @Test
  def namedParameter = new FileSet {
    """
    class NamedParameter {
      def foo(/*(*/b/*)*/: Int, c: String) {
        println(b)
      }
      foo(c = "", b = 5)
    }
    """ becomes
    """
    class NamedParameter {
      def foo(/*(*/xys/*)*/: Int, c: String) {
        println(xys)
      }
      foo(c = "", xys = 5)
    }
    """
  } applyRefactoring(renameTo("xys"))

  @Ignore
  @Test
  def namedParameterAndDefault = new FileSet {
    """
    class NamedParameter {
      def foo(/*(*/b/*)*/: Int, c: String = "") {
        println(b)
      }
      foo(b = 5)
    }
    """ becomes
    """
    class NamedParameter {
      def foo(/*(*/xys/*)*/: Int, c: String = "") {
        println(xys)
      }
      foo(xys = 5)
    }
    """
  } applyRefactoring(renameTo("xys"))

  @Ignore
  @Test
  def namedParameterInDeclaredOrder = new FileSet {
    """
    class NamedParameter {
      def foo(/*(*/b/*)*/: Int, c: String) {
        println(b)
      }
      foo(b = 5, c = "")
    }
    """ becomes
    """
    class NamedParameter {
      def foo(/*(*/xys/*)*/: Int, c: String) {
        println(xys)
      }
      foo(xys = 5, c = "")
    }
    """
  } applyRefactoring(renameTo("xys"))

  @Ignore
  @Test
  def namedParameterInSecondArgsList = new FileSet {
    """
    class NamedParameter {
      def foo(x: Int)(/*(*/b/*)*/: Int, c: String) {
        println(b)
      }
      foo(5)(b = 5, c = "")
    }
    """ becomes
    """
    class NamedParameter {
      def foo(x: Int)(/*(*/xys/*)*/: Int, c: String) {
        println(xys)
      }
      foo(5)(xys = 5, c = "")
    }
    """
  } applyRefactoring(renameTo("xys"))

  @Ignore
  @Test
  def updateMethodAndNamedArgument = new FileSet {
    """
    class Updateable { def update(/*(*/what/*)*/: Int, rest: Int) = 0 }

    class NamedParameter {
      val up = new Updateable
      up(what = 1) = 2
    }
    """ becomes
    """
    class Updateable { def update(/*(*/xys/*)*/: Int, rest: Int) = 0 }

    class NamedParameter {
      val up = new Updateable
      up(xys = 1) = 2
    }
    """
  } applyRefactoring(renameTo("xys"))

  @Test
  def privatePrimaryConstructor = new FileSet {
    """
    class /*(*/SomeClass/*)*/ private () extends AnyRef {
      def meta = SomeClass
    }
    object SomeClass extends SomeClass
    """ becomes
    """
    class /*(*/RenamedClass/*)*/ private () extends AnyRef {
      def meta = RenamedClass
    }
    object RenamedClass extends RenamedClass
    """
  } applyRefactoring(renameTo("RenamedClass"))

  @Test
  def constructionInforComprehension = new FileSet {
    """
    package constructorInForComprehension
    case class /*(*/A/*)*/(val x: Int)

    object Foo {
      def doit = for (A(x) <- Seq(A(1), A(2))) yield x
    }
    """ becomes
    """
    package constructorInForComprehension
    case class /*(*/BBB/*)*/(val x: Int)

    object Foo {
      def doit = for (BBB(x) <- Seq(BBB(1), BBB(2))) yield x
    }
    """
  } applyRefactoring(renameTo("BBB"))

  @Test
  def privateMembersTupleNotation = new FileSet {
    """
    package privateMembersTupleNotation
    class /*(*/Test/*)*/ {

      private val A, B, C = this
    }
    """ becomes
    """
    package privateMembersTupleNotation
    class /*(*/MyTest/*)*/ {

      private val A, B, C = this
    }
    """
  } applyRefactoring(renameTo("MyTest"))

  @Test
  def renameMethodForComprehensionBody = new FileSet {
    """
    package renameMethodForComprehensionBody
    class Testing {
      def isBlank[T](t: T) = false
      object elt {
        val child = List(1,2,3)
      }

      def /*(*/processContent/*)*/ {
        for(node <- elt.child if !isBlank(node)) {
          val element = new Array(node)
        }
      }
    }
    """ becomes
    """
    package renameMethodForComprehensionBody
    class Testing {
      def isBlank[T](t: T) = false
      object elt {
        val child = List(1,2,3)
      }

      def /*(*/getContent/*)*/ {
        for(node <- elt.child if !isBlank(node)) {
          val element = new Array(node)
        }
      }
    }
    """
  } applyRefactoring(renameTo("getContent"))
}
