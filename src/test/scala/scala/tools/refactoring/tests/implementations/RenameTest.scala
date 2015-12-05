/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import scala.language.existentials
import scala.language.reflectiveCalls
import scala.tools.refactoring.common.Change
import scala.tools.refactoring.common.TracingImpl

import implementations.Rename
import tests.util.TestHelper
import tests.util.TestHelper.PrepResultWithChanges
import tests.util.TestRefactoring

class RenameTest extends TestHelper with TestRefactoring {
  outer =>

  private def prepareAndRenameTo(name: String)(pro: FileSet): PrepResultWithChanges = {
    val impl = new TestRefactoringImpl(pro) {
      val refactoring = new Rename with TracingImpl with TestProjectIndex
    }
    PrepResultWithChanges(Some(impl.preparationResult()), impl.performRefactoring(name))
  }

  private def renameTo(name: String)(pro: FileSet): List[Change] = {
    prepareAndRenameTo(name)(pro).changes
  }

  protected override def nestTestsInUniqueBasePackageByDefault = true

  /*
   * See Assembla Ticket 1002537
   */
  @Test
  def testRenameObjectWithInnerClass1002537Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    object /*(*/RenameMe/*)*/ {
      class Class
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    object /*(*/X/*)*/ {
      class Class
    }
    """ -> TaggedAsGlobalRename;

    """
    package com.github.mlangc.experiments.bug

    import com.github.mlangc.experiments.RenameMe.Class

    class Bug {
      def x: Class = ???
    }
    """ becomes
    """
    package com.github.mlangc.experiments.bug

    import com.github.mlangc.experiments.X.Class

    class Bug {
      def x: Class = ???
    }
    """
  } prepareAndApplyRefactoring(prepareAndRenameTo("X"))

  @Test
  def renameOverlapping() = new FileSet {
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
  def renameRecursive() = new FileSet {
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
  def renameLocalValue() = new FileSet {
    """
      package renameLocal1
      class A {
        def double(s: String) = s + s
        def extractFrom: Unit = {
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
        def extractFrom: Unit = {
          val b = "hallo"
  /*(*/   b   /*)*/ .length
          double(b + "a")
        }
      }
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameLazyVal() = new FileSet {
    """
    package renameLazyVal
    class A {
      def rename(): Unit = {
        lazy val a = 5
        println(/*(*/a/*)*/)
      }
    }
    """ becomes
    """
    package renameLazyVal
    class A {
      def rename(): Unit = {
        lazy val b = 5
        println(/*(*/b/*)*/)
      }
    }
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameLazyValFromDefinition() = new FileSet {
    """
    package renameLazyVals
    class A {
      def print: Unit = {
        lazy val /*(*/a = 42/*)*/
        println(a)
      }
    }
    """ becomes
    """
    package renameLazyVals
    class A {
      def print: Unit = {
        lazy val /*(*/c = 42/*)*/
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  /*
   * See Assembla ticket #1002392
   */
  @Test
  def renameLazyValFromClass() = new FileSet {
    """
    package renameLazyValFromClass
    class Bug {
      lazy val /*(*/tryRenameMe/*)*/ = "bar"
    }
    """ becomes
    """
    package renameLazyValFromClass
    class Bug {
      lazy val /*(*/test/*)*/ = "bar"
    }
    """
  } applyRefactoring(renameTo("test"))

  /*
   * See Assembla ticket #1002392
   */
  @Test
  def renameProtectedLazyValFromClass() = new FileSet {
    """
    package renameLazyValFromClass
    class Bug {
      protected lazy val /*(*/tryRenameMe/*)*/ = "bar"
    }
    """ becomes
    """
    package renameLazyValFromClass
    class Bug {
      protected lazy val /*(*/test/*)*/ = "bar"
    }
    """
  } applyRefactoring(renameTo("test"))

  /*
   * See Assembla ticket #1002392
   */
  @Test
  def renameLazyValFromClassWithOneLetterName() = new FileSet {
    """
    package renameLazyValFromClass
    class Bug {
      lazy val /*(*/x/*)*/ = "bar"
    }
    """ becomes
    """
    package renameLazyValFromClass
    class Bug {
      lazy val /*(*/y/*)*/ = "bar"
    }
    """
  } applyRefactoring(renameTo("y"))

  @Test
  def renameParameterWithoutSelection() = new FileSet {
    """
    package renameParameter
    class A {
      def rename(a/*<-*/: String): Unit = {
        println(a)
      }
    }
    """ becomes
    """
    package renameParameter
    class A {
      def rename(b/*<-*/: String): Unit = {
        println(b)
      }
    }
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameParameter() = new FileSet {
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
  def renameWithType() = new FileSet {
    """
    package renameWithType
    class A {
      def rename(a: String): Unit = {
        a match {
          case b: String => /*(*/  b  /*)*/
        }
      }
    }
    """ becomes
    """
    package renameWithType
    class A {
      def rename(a: String): Unit = {
        a match {
          case c: String => /*(*/  c  /*)*/
        }
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameMultiAssignment() = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val (/*(*/a/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val (/*(*/c/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameMultiAssignmentWithTA() = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val (/*(*/a: Int/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val (/*(*/c: Int/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameClassWithTypeParameters() = new FileSet {
    """
    case class /*(*/Test/*)*/[A, B](a:A,b:B)
    """ becomes
    """
    case class /*(*/Test1/*)*/[A, B](a:A,b:B)
    """
  } applyRefactoring(renameTo("Test1"))

  @Test
  def renameAbstractType() = new FileSet {
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
  def renameReferenceToOuterclass() = new FileSet {
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
  def renameDeconstructingAssignment() = new FileSet {
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val List(/*(*/a/*)*/, b) = List(5, 6)
        println(a + b)
      }
    }
    """ becomes
    """
    package renameMultiAssignment
    class A {
      def print: Unit = {
        val List(/*(*/c/*)*/, b) = List(5, 6)
        println(c + b)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameBinding() = new FileSet {
    """
    package renameBinding
    class A {
      def print: Unit = {
        1 match { case /*(*/ i /*)*/ => i }
      }
    }
    """ becomes
    """
    package renameBinding
    class A {
      def print: Unit = {
        1 match { case /*(*/ integer /*)*/ => integer }
      }
    }
    """
  } applyRefactoring(renameTo("integer"))

  @Test
  def renameNewVal() = new FileSet {
    """
    package renameNewVal
    class A(i: Int) {
      def print: Unit = {
        var  /*(*/  l = /*)*/  new A(5)
      }
    }
    """ becomes
    """
    package renameNewVal
    class A(i: Int) {
      def print: Unit = {
        var  /*(*/  ls = /*)*/  new A(5)
      }
    }
    """
  } applyRefactoring(renameTo("ls"))

  @Test
  def renameLazyArg() = new FileSet {
    """
    package renameLazyArg
    class A(i: Int) {
      def print(a: => String): Unit = {
        println(/*(*/  a  /*)*/)
      }
    }
    """ becomes
    """
    package renameLazyArg
    class A(i: Int) {
      def print(s: => String): Unit = {
        println(/*(*/  s  /*)*/)
      }
    }
    """
  } applyRefactoring(renameTo("s"))

  @Test
  def forComprehension() = new FileSet {
    """
    package forComprehension
    class A {
      def print: Unit = {
        for(  /*(*/  i  /*)*/  <- 1 to 10) yield i
      }
    }
    """ becomes
    """
    package forComprehension
    class A {
      def print: Unit = {
        for(  /*(*/  index  /*)*/  <- 1 to 10) yield index
      }
    }
    """
  } applyRefactoring(renameTo("index"))

  @Test
  def inConstructor() = new FileSet {
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
  def renameMethod() = new FileSet {
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
  def renameMethodInMultipleFiles() = new FileSet {
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
  def renameClass() = new FileSet {
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
  def renameTypeParameter() = new FileSet {
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
  def renameSuperClass() = new FileSet {
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
  def renameType() = new FileSet {
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/Person/*)*/(name: String)

      def main(args: Array[String]): Unit = {

        val people: List[Person] = List(new Person("Mirko"), new Person("Christina"))

      }
    }""" becomes
    """

    class Person(name: String)

    object Rename1 {
      class /*(*/P/*)*/(name: String)

      def main(args: Array[String]): Unit = {

        val people: List[P] = List(new P("Mirko"), new P("Christina"))

      }
    }"""
  } applyRefactoring(renameTo("P"))

  @Test
  def renameCaseClass() = new FileSet {
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
  def renameSelfType() = new FileSet {
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
  def renameSelfType2() = new FileSet {
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
  def renameClassWithImport() = new FileSet {
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
  def renamePackages() = new FileSet {
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
  def renameInnerPackages() = new FileSet {
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
  def renameUnderWindows() = new FileSet {
    "package com.test\r\nobject Hello {\r\n  def test = {\r\n    val /*(*/loc/*)*/ = 42\r\n    loc * loc\r\n  }\r\n}" becomes
    "package com.test\r\nobject Hello {\r\n  def test = {\r\n    val /*(*/fourtytwo/*)*/ = 42\r\n    fourtytwo * fourtytwo\r\n  }\r\n}"
  } applyRefactoring(renameTo("fourtytwo"))


  @Test
  def renameCaseClassAndObject() = new FileSet {
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
  def renameNestedType() = new FileSet {
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
  def renameIdentifierWithBackticks() = new FileSet {
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
  def renameClassWithBackTicks() = new FileSet {
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
  def renameTypeParamInSecondConstructor() = new FileSet {
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
  def renameSingleVariableDeconstructingAssignment() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment
    class A {
      def print: Unit = {
        val List(/*(*/a/*)*/, _) = List(5, 6)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment
    class A {
      def print: Unit = {
        val List(/*(*/c/*)*/, _) = List(5, 6)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment2() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print: Unit = {
        val Some(/*(*/a/*)*/) = Some(6)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print: Unit = {
        val Some(/*(*/c/*)*/) = Some(6)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment3() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print: Unit = {
        val Reg = "(\\w)".r
        val Reg(/*(*/a/*)*/) = "x"
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment2
    class A {
      def print: Unit = {
        val Reg = "(\\w)".r
        val Reg(/*(*/c/*)*/) = "x"
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment4() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment4
    class A {
      def print: Unit = {
        val List(_, /*(*/a/*)*/) = List(1, 2)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment4
    class A {
      def print: Unit = {
        val List(_, /*(*/c/*)*/) = List(1, 2)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment5() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment5
    class A {
      def print: Unit = {
        val List(_, Some(List(_, Some(/*(*/a/*)*/), _))) = List(None, Some(List(1, Some(2), 3)))
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment5
    class A {
      def print: Unit = {
        val List(_, Some(List(_, Some(/*(*/c/*)*/), _))) = List(None, Some(List(1, Some(2), 3)))
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))

  @Test
  def renameSingleVariableDeconstructingAssignment6() = new FileSet {
    """
    package renameSingleVariableDeconstructingAssignment6
    class A {
      def print: Unit = {
        val List(/*(*/a/*)*/: Int) = List(42)
        println(a)
      }
    }
    """ becomes
    """
    package renameSingleVariableDeconstructingAssignment6
    class A {
      def print: Unit = {
        val List(/*(*/c/*)*/: Int) = List(42)
        println(c)
      }
    }
    """
  } applyRefactoring(renameTo("c"))


  @Test
  def renameClassParameterPassedIntoSuperClassWithExpression() = new FileSet {
    """
    class Class(/*(*/a/*)*/: String) extends RuntimeException(a + "")
    """ becomes
    """
    class Class(/*(*/b/*)*/: String) extends RuntimeException(b + "")
    """
  } applyRefactoring(renameTo("b"))

  @Test
  def renameClassParameterPassedIntoSuperClassWithExpression2() = new FileSet {
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
  def renameSuperclassAtEndOfFile() = new FileSet {
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
  def renameSuperclassAtEndOfFile2() = new FileSet {
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
  def renameMethodInCaseObject() = new FileSet {
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
  def renameClassWithClassOfUsage() = new FileSet {
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
  def renameClassExplicitSelfTypeAnnotation() = new FileSet {
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
  def renameWithMultipleContextBounds() = new FileSet {
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
  def renameClassWithThisConstuctorCall() = new FileSet {
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
  def renameAbstractTypesInHierarchy() = new FileSet {
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
  def renameClassSelfTypeAnnotation() = new FileSet {
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
  def renameClassSelfTypeAnnotation2() = new FileSet {
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
  def renameMethodWithContextBound() = new FileSet {
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
  def coloncolon() = new FileSet {
    """
    object Problem07 {
      def /*(*/flatten/*)*/(list: List[Any]): List[Any] = list match {
        case (head@_::_)::tail => flatten(head) ++ flatten(tail)
        case Nil::tail => flatten(tail)
        case head::tail => head :: flatten(tail)
        case Nil => Nil
      }
    }
    """ becomes
    """
    object Problem07 {
      def /*(*/fltn/*)*/(list: List[Any]): List[Any] = list match {
        case (head@_::_)::tail => fltn(head) ++ fltn(tail)
        case Nil::tail => fltn(tail)
        case head::tail => head :: fltn(tail)
        case Nil => Nil
      }
    }
    """
  } applyRefactoring(renameTo("fltn"))

  @Test
  def typeProjection() = new FileSet {
    """
    class A {
      trait /*(*/B/*)*/
      def foo(b: A#B): Unit = {}
    }
    """ becomes
    """
    class A {
      trait /*(*/C/*)*/
      def foo(b: A#C): Unit = {}
    }
    """
  } applyRefactoring(renameTo("C"))

  @Test
  def overriddenMethod() = new FileSet {
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
  def namedParameter() = new FileSet {
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
  def namedParameterAndDefault() = new FileSet {
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
  def namedParameterInDeclaredOrder() = new FileSet {
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
  def namedParameterInSecondArgsList() = new FileSet {
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
  def updateMethodAndNamedArgument() = new FileSet {
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
  def privatePrimaryConstructor() = new FileSet {
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
  def constructionInforComprehension() = new FileSet {
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
  def privateMembersTupleNotation() = new FileSet {
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
  def renameMethodForComprehensionBody() = new FileSet {
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

  @Test
  def renameApplyCall() = new FileSet {
    """
    package renameApplyCall
    object Foo {
      val /*(*/xs/*)*/ = List(1,2,3)
      def bar(): Unit = {
        println(xs(0))
        println(xs)

      }
    }
    """ becomes
    """
    package renameApplyCall
    object Foo {
      val /*(*/list/*)*/ = List(1,2,3)
      def bar(): Unit = {
        println(list(0))
        println(list)

      }
    }
    """
  } applyRefactoring(renameTo("list"))

  @Test
  def renameFinalVal() = new FileSet {
    """
    package renameFinalVal

    object X {
      final val /*(*/Value/*)*/ = 0
    }
    """ becomes
    """
    package renameFinalVal

    object X {
      final val /*(*/x/*)*/ = 0
    }
    """;
    """
    package renameFinalVal

    class ClassA {
      val value = X.Value
    }
    """ becomes """
    package renameFinalVal

    class ClassA {
      val value = X.x
    }
    """
  } applyRefactoring(renameTo("x"))

  /*
   * See Assembla ticket #1001928
   */
  @Test
  def testRenameClassInSamePackageButOtherFile() = new FileSet {
    """
    class A
    """ -> "A.scala" becomes
    """
    class B
    """ -> "B.scala";

    """
    class C extends /*(*/A/*)*/
    """ becomes
    """
    class C extends /*(*/B/*)*/
    """
  } applyRefactoring(renameTo("B"))


  /*
   * See Assembla ticket #1001928
   */
  @Test
  def testRenameClassInDifferentPackageAndOtherFile() = new FileSet {
    """
    package one
    class Eins
    """ -> "Eins.scala" becomes
    """
    package one
    class One
    """ -> "One.scala";

    """
    package two
    import one._

    class Two {
      val uno = new /*(*/Eins/*)*/
    }
    """ becomes
    """
    package two
    import one._

    class Two {
      val uno = new /*(*/One/*)*/
    }
    """
  } applyRefactoring(renameTo("One"))

  @Test
  def testClassInDedicatedFile() = new FileSet {
    "class /*(*/Foo/*)*/" -> "Foo.scala" becomes
    "class /*(*/Bazius/*)*/" -> "Bazius.scala"
  } applyRefactoring(renameTo("Bazius"))

  /*
   * See Assembla ticket #1002435
   */
  @Test
  def testRenamePublicValInPlainClass() = new FileSet {
    """
    class Bug(val /*(*/number/*)*/: Int)
    """ becomes
    """
    class Bug(val /*(*/z/*)*/: Int)
    """;

    """
    object Buggy {
      def x = new Bug(32).number
    }
    """ becomes
    """
    object Buggy {
      def x = new Bug(32).z
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  /*
   * See Assembla ticket #1002435
   */
  @Test
  def testRenamePublicValInCaseClass() = new FileSet {
    """
    case class Bug(/*(*/number/*)*/: Int)
    """ becomes
    """
    case class Bug(/*(*/z/*)*/: Int)
    """;

    """
    object Buggy {
      def x = Bug(32).number
    }
    """ becomes
    """
    object Buggy {
      def x = Bug(32).z
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  /*
   * See Assembla ticket #1002435
   */
  @Test
  def testRenamePublicValInTrait() = new FileSet {
    """
    trait Bug {
      val /*(*/number/*)*/: Int
    }
    """ becomes
    """
    trait Bug {
      val /*(*/x/*)*/: Int
    }
    """;

    """
    object Buggy {
      def bug: Bug = ???
      def x = bug.number
    }
    """ becomes
    """
    object Buggy {
      def bug: Bug = ???
      def x = bug.x
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("x"))

  @Test
  def testRenamePublicVarInPlainClass() = new FileSet {
    """
    class Bug(var /*(*/number/*)*/: Int)
    """ becomes
    """
    class Bug(var /*(*/z/*)*/: Int)
    """;

    """
    object Buggy {
      def x = new Bug(32).number
    }
    """ becomes
    """
    object Buggy {
      def x = new Bug(32).z
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  @Test
  def testRenamePrivateVarInPlainClass() = new FileSet {
    """
    class Bug {
      private var /*(*/number/*)*/ = 42
      def test = number
    }
    """ becomes
    """
    class Bug {
      private var /*(*/z/*)*/ = 42
      def test = z
    }
    """ -> TaggedAsLocalRename;
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  @Test
  def testRenamePkgPrivateVal() = new FileSet {
    """
    package test
    class Bug(private[test] val /*(*/number/*)*/: Int)
    """ becomes
    """
    package test
    class Bug(private[test] val /*(*/z/*)*/: Int)
    """;

    """
    package test
    object Buggy {
      def x = new Bug(32).number
    }
    """ becomes
    """
    package test
    object Buggy {
      def x = new Bug(32).z
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  /*
   * See Assembla Ticket #1002446
   */
  @Test
  def testRenamePkgPrivateDef() = new FileSet {
    """
    package bug
    class Bug {
      private[bug] def /*(*/bar/*)*/ = 99
    }
    """ becomes
    """
    package bug
    class Bug {
      private[bug] def /*(*/x/*)*/ = 99
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("x"))

  @Test
  def testRenamePrivateThisVal() = new FileSet {
    """
    class Bug {
      private[this] val /*(*/nautilus/*)*/ = 99
    }
    """ becomes
    """
    class Bug {
      private[this] val /*(*/z/*)*/ = 99
    }
    """ -> TaggedAsLocalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  @Test
  def testRenameValWithCommentAfterModifier() = new FileSet {
    """
    class Bug {
      private/*--*/ val /*(*/nautilus/*)*/ = 99
    }
    """ becomes
    """
    class Bug {
      private/*--*/ val /*(*/z/*)*/ = 99
    }
    """ -> TaggedAsLocalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  @Test
  def testRenamePkgPrivateValWithComments() = new FileSet {
    """
    package bug
    class Bug {
      private/*--*/ //**//**//**//**//**/
      // -/**/-
      // -/**/-
      [/**/ bug /**/] val /*(*/nautilus/*)*/ = 99
    }
    """ becomes
    """
    package bug
    class Bug {
      private/*--*/ //**//**//**//**//**/
      // -/**/-
      // -/**/-
      [/**/ bug /**/] val /*(*/z/*)*/ = 99
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  @Test
  def testRenamePkgProtectedDefWithComments() = new FileSet {
    """
    package bug
    class Bug {
      protected/*--*/ //**//**//**//**//**/
      ////
      // -/**/-
      // -/**/-
      [/**/ bug /**/] /**/ def /*(*/nautilus/*)*/ = 99
    }
    """ becomes
    """
    package bug
    class Bug {
      protected/*--*/ //**//**//**//**//**/
      ////
      // -/**/-
      // -/**/-
      [/**/ bug /**/] /**/ def /*(*/z/*)*/ = 99
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("z"))

  /*
   * Correctly renaming package private lazy vals is not as easy as one might hope,
   * because of their representation in the ASTs, both as "ValDef"s and "DefDef"s.
   */
  @Ignore
  @Test
  def testRenamePkgProtectedLazyVal() = new FileSet {
    """
    package experiments
    class Clazz {
      private[experiments] lazy val /*(*/x/*)*/ = 999
    }""" becomes
    """
    package experiments
    class Clazz {
      private[experiments] lazy val /*(*/xy/*)*/ = 999
    }""" -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("xy"))


  /*
   * See Assembla Ticket 1002434
   */
  @Test
  def testRenameOverrideVal() = new FileSet {
    """
    trait Bug {
      def x = 42
    }
    """ becomes
    """
    trait Bug {
      def xyz = 42
    }
    """;

    """
    class Buggy extends Bug {
      override val x = 43
    }
    """ becomes
    """
    class Buggy extends Bug {
      override val xyz = 43
    }
    """;

    """
    class MoreBugs extends Buggy {
      override val /*(*/x/*)*/ = 99
    }
    """ becomes
    """
    class MoreBugs extends Buggy {
      override val /*(*/xyz/*)*/ = 99
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("xyz"))

  /*
   * See Assembla Ticket 1002490
   */
  @Test
  def testRenameClassWithCompanion() = new FileSet(expectCompilingCode = false) {
    """
    class /*(*/Bug/*)*/
    object Bug
    """ -> "Bug.scala" becomes
    """
    class /*(*/Buggy/*)*/
    object Buggy
    """ -> "Buggy.scala"
  } applyRefactoring(renameTo("Buggy"))

  /*
   * See Assembla Ticket 1002436
   */
  @Test
  def testRenameTypeAnnotatingLazyVal() = new FileSet {
    """
    class /*(*/Bug/*)*/

    class Buggy {
      lazy val bug: Bug = new Bug

      def moreBugs = {
        lazy val buggy: Bug = new Bug
        val notBuggy: Bug = new Bug
        buggy.hashCode + notBuggy.hashCode
      }
    }
    """ becomes
    """
    class /*(*/Mistkaefer/*)*/

    class Buggy {
      lazy val bug: Mistkaefer = new Mistkaefer

      def moreBugs = {
        lazy val buggy: Mistkaefer = new Mistkaefer
        val notBuggy: Mistkaefer = new Mistkaefer
        buggy.hashCode + notBuggy.hashCode
      }
    }
    """
  } applyRefactoring(renameTo("Mistkaefer"))

  @Test
  def testRenameTypeAnnotatingLazyValMinimal() = new FileSet {
    """
    object Buggy {
      class /*(*/Bug/*)*/
      def err = {
        lazy val bug: Bug = new Bug
        bug.hashCode
      }
    }
    """ becomes
    """
    object Buggy {
      class /*(*/Mistkaefer/*)*/
      def err = {
        lazy val bug: Mistkaefer = new Mistkaefer
        bug.hashCode
      }
    }
    """
  } applyRefactoring(renameTo("Mistkaefer"))

  /*
   * See Assembla Ticket #1002498
   */
  @Test
  def testRenameProtectedOverrideDef() = new FileSet {
    """
    trait Bug {
      protected def /*(*/br0ken/*)*/: Int
    }

    class Buggy extends Bug {
      protected override def br0ken = 9
    }
    """ becomes
    """
    trait Bug {
      protected def /*(*/ups/*)*/: Int
    }

    class Buggy extends Bug {
      protected override def ups = 9
    }
    """
  } applyRefactoring(renameTo("ups"))

  /*
   * See Assembla Ticket #1002498
   */
  @Test
  def testRenameProtectedOverrideVal() = new FileSet {
    """
    trait Bug {
      protected val /*(*/br0ken/*)*/: Int
    }

    class Buggy extends Bug {
      protected override val br0ken = 9
    }
    """ becomes
    """
    trait Bug {
      protected val /*(*/ups/*)*/: Int
    }

    class Buggy extends Bug {
      protected override val ups = 9
    }
    """
  } applyRefactoring(renameTo("ups"))

  @Test
  def testRenameOverrideProtected() = new FileSet {
    """
    trait Base {
      protected def /*(*/x/*)*/: Int
    }

    class Derived1 extends Base {
      override protected val x = 9
    }

    class Derived2 extends Base {
      override protected def x = 9
    }
    """ becomes
    """
    trait Base {
      protected def /*(*/xxx/*)*/: Int
    }

    class Derived1 extends Base {
      override protected val xxx = 9
    }

    class Derived2 extends Base {
      override protected def xxx = 9
    }
    """
  } applyRefactoring(renameTo("xxx"))

  /*
   * See Assembla Ticket #1002540
   */
  @Test
  def testRenameInCaseClassWithCopy1002540Ex1() = new FileSet {
    """
    case class Bug(i: Int = 1, j: Int = 2) {
      def /*(*/buggy/*)*/ = copy(j = i)
    }
    """ becomes
    """
    case class Bug(i: Int = 1, j: Int = 2) {
      def /*(*/notBuggy/*)*/ = copy(j = i)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex2() = new FileSet {
    """
    case class Bug2(s1: String = "", s2: String = "") {
      def /*(*/buggy/*)*/ = copy(s2 = "s1", s1 = "s2")
    }
    """ becomes
    """
    case class Bug2(s1: String = "", s2: String = "") {
      def /*(*/notBuggy/*)*/ = copy(s2 = "s1", s1 = "s2")
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex3() = new FileSet {
    """
    case class Bug3(s1: String = "", s2: String = "", ss1: String = "", ss2: String = "") {
      def /*(*/buggy/*)*/ = copy(s2 = ss1, s1 = ss2)
    }
    """ becomes
    """
    case class Bug3(s1: String = "", s2: String = "", ss1: String = "", ss2: String = "") {
      def /*(*/notBuggy/*)*/ = copy(s2 = ss1, s1 = ss2)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex4() = new FileSet {
    """
    case class Bug4(i: Int = 1, j: Int = 2, `i!`: Int = 3, `j!`: Int = 4) {
      def /*(*/buggy/*)*/ = copy(j = `i!`, i = `j!`)
    }
    """ becomes
    """
    case class Bug4(i: Int = 1, j: Int = 2, `i!`: Int = 3, `j!`: Int = 4) {
      def /*(*/notBuggy/*)*/ = copy(j = `i!`, i = `j!`)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex5() = new FileSet {
    """
    case class Bug5(i: Int = 1, j: Int = 2) {
      def /*(*/buggy/*)*/ = copy(j = i, i = j)
    }
    """ becomes
    """
    case class Bug5(i: Int = 1, j: Int = 2) {
      def /*(*/notBuggy/*)*/ = copy(j = i, i = j)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex6() = new FileSet {
    """
    case class Bug6(i: Int = 1, j: Int = 2) {
      def /*(*/buggy/*)*/ = copy(j = /*i*/0, i = /*j*/1)
    }
    """ becomes
    """
    case class Bug6(i: Int = 1, j: Int = 2) {
      def /*(*/notBuggy/*)*/ = copy(j = /*i*/0, i = /*j*/1)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex7() = new FileSet {
    """
    case class Bug7(i: Int = 1, j: Int = 2) {
      private val `i=3` = 666
      def /*(*/buggy/*)*/ = copy(j = /*i*/`i=3`, i = /*j*/1)
    }
    """ becomes
    """
    case class Bug7(i: Int = 1, j: Int = 2) {
      private val `i=3` = 666
      def /*(*/notBuggy/*)*/ = copy(j = /*i*/`i=3`, i = /*j*/1)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex8() = new FileSet {
    """
    case class Bug8(`i=j`: Int = 667, `i`: Int = 23) {
      def /*(*/buggy/*)*/ = copy(`i=j`=666,`i`=1)
    }
    """ becomes
    """
    case class Bug8(`i=j`: Int = 667, `i`: Int = 23) {
      def /*(*/notBuggy/*)*/ = copy(`i=j`=666,`i`=1)
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  @Test
  def testRenameInCaseClassWithCopy100254Ex9() = new FileSet {
    """
    case class Bug9(i: Int, j: Int) {
      def /*(*/buggy/*)*/ = copy(j = {
        val i = 3
        i
      })
    }
    """ becomes
    """
    case class Bug9(i: Int, j: Int) {
      def /*(*/notBuggy/*)*/ = copy(j = {
        val i = 3
        i
      })
    }
    """
  } applyRefactoring(renameTo("notBuggy"))

  /*
   * See Assembla Ticket 1002560
   */
  @Test
  def testRenameTraitMethod1002560Ex1() = new FileSet {
    """
    package test

    trait Bug {
      def /*(*/renameMe/*)*/: Seq[Bug]
    }
    """ becomes
    """
    package test

    trait Bug {
      def /*(*/ups/*)*/: Seq[Bug]
    }
    """
  } applyRefactoring(renameTo("ups"))

  @Test
  def testRenameTraitMethod1002560Ex2() = new FileSet {
    """
    trait Bug2 {
      def /*(*/renameMe/*)*/: Map[Int, Int]
    }
    """ becomes
    """
    trait Bug2 {
      def /*(*/ups/*)*/: Map[Int, Int]
    }
    """
  } applyRefactoring(renameTo("ups"))

  @Test
  def testRenameTraitMethod1002560Ex3() = new FileSet {
    """
    trait Bug3 {
      def /*(*/renameMe/*)*/: Seq[/**/Int]
    }
    """ becomes
    """
    trait Bug3 {
      def /*(*/ups/*)*/: Seq[/**/Int]
    }
    """
  } applyRefactoring(renameTo("ups"))

  @Test
  def testRenameTraitMethod1002560Ex4() = new FileSet {
    """
    trait Bug4 {
      def /*(*/renameMe/*)*/: Seq[Int ]
    }
    """ becomes
    """
    trait Bug4 {
      def /*(*/ups/*)*/: Seq[Int ]
    }
    """
  } applyRefactoring(renameTo("ups"))

  @Test
  def testRenameTraitMethod1002560Ex5() = new FileSet {
    """
    trait Bug5 {
      def /*(*/renameMe/*)*/: Seq[/**/ Int /* --- */ ]
    }
    """ becomes
    """
    trait Bug5 {
      def /*(*/ups/*)*/: Seq[/**/ Int /* --- */ ]
    }
    """
  } applyRefactoring(renameTo("ups"))

  /*
   * See Assembla Ticket 1002371
   */
  @Test
  def testRenameWithDifferentValsWithSameName1002371Ex1() = new FileSet {
    """
    class C {
      val /*(*/value/*)*/ = 0
      def f = {
        val value = 0
        println(value)
        this
      }
    }
    """ becomes
    """
    class C {
      val /*(*/ups/*)*/ = 0
      def f = {
        val value = 0
        println(value)
        this
      }
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameWithDifferentValsWithSameName1002371Ex2() = new FileSet {
    """
    class C {
      val value = 0
      def f = {
        val /*(*/value/*)*/ = 0
        println(value)
        this
      }
    }
    """ becomes
    """
    class C {
      val value = 0
      def f = {
        val /*(*/ups/*)*/ = 0
        println(ups)
        this
      }
    }
    """ -> TaggedAsLocalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameWithDifferentValsWithSameName1002371Ex3() = new FileSet {
    """
    class C {
      val value = 0
      def f = {
        val /*(*/value/*)*/ = 0
        println(value)
      }
    }
    """ becomes
    """
    class C {
      val value = 0
      def f = {
        val /*(*/ups/*)*/ = 0
        println(ups)
      }
    }
    """ -> TaggedAsLocalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameWithDifferentValsWithSameName1002371Ex4() = new FileSet {
    """
    class C {
      val /*(*/value/*)*/ = 0
      def f = {
        val value = 0
        println(value)
        this
      }

     val another = value
    }
    """ becomes
    """
    class C {
      val /*(*/ups/*)*/ = 0
      def f = {
        val value = 0
        println(value)
        this
      }

     val another = ups
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  /*
   * See Assembla Ticket 1002569
   */
  @Test
  def testRenameLazyConstants1002569Ex1() = new FileSet {
    """
    object Bug1 {
      lazy val l = 1
      lazy val /*(*/tryRenameMe/*)*/ = 2

    }
    """ becomes
    """
    object Bug1 {
      lazy val l = 1
      lazy val /*(*/renamed/*)*/ = 2

    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("renamed"))

  @Test
  def testRenameLazyConstants1002569Ex2() = new FileSet {
    """
    package test

    object Bug2 {
      lazy val l1 = mkVal(1)
      lazy val l2 = mkVal(2)
      lazy val /*(*/tryRenameMe/*)*/ = mkVal(3)

      def mkVal(i: Int) = i
    }
    """ becomes
    """
    package test

    object Bug2 {
      lazy val l1 = mkVal(1)
      lazy val l2 = mkVal(2)
      lazy val /*(*/renamed/*)*/ = mkVal(3)

      def mkVal(i: Int) = i
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("renamed"))

  @Test
  def testRenameLazyConstants1002569Ex3() = new FileSet {
    """
    package com.github.mlangc.experiments

    object Bystander {
      lazy val l = "1"
    }

    object Bug3 {
      lazy val l = 0
      lazy val /*(*/tryRenameMe/*)*/ = 0
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    object Bystander {
      lazy val l = "1"
    }

    object Bug3 {
      lazy val l = 0
      lazy val /*(*/renamed/*)*/ = 0
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("renamed"))

  /*
   * See Assembla Ticket 1002502
   */
  @Test
  def testRenameWithOverridingLazyVal1002502Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    abstract class Bug {
      def /*(*/renameMe/*)*/: Int
    }

    class Buggy extends Bug {
      lazy val renameMe = 99
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    abstract class Bug {
      def /*(*/x/*)*/: Int
    }

    class Buggy extends Bug {
      lazy val x = 99
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("x"))

  /*
   * See Assembla Ticket 1002564
   */
  @Test
  def testRenameWithDefaultArgs1002564Ex1() = new FileSet {
    """
    object X extends App {
      O().test.meth()
    }
    class C {
      def meth(j: Int = 0) = j
    }
    class O {
      def /*(*/test/*)*/: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ becomes
    """
    object X extends App {
      O().test2.meth()
    }
    class C {
      def meth(j: Int = 0) = j
    }
    class O {
      def /*(*/test2/*)*/: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("test2"))

  @Test
  def testRenameSimilarTo1002564Ex1WithoutDefaultArgs() = new FileSet {
    """
    object X extends App {
      O().test.meth()
    }
    class C {
      def meth() = 0
    }
    class O {
      def /*(*/test/*)*/: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ becomes
    """
    object X extends App {
      O().test2.meth()
    }
    class C {
      def meth() = 0
    }
    class O {
      def /*(*/test2/*)*/: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("test2"))

  @Test
  def testRenameWithDefaultArgs1002564Ex2() = new FileSet {
    """
    object X extends App {
      O().test.meth()
    }
    class C {
      def /*(*/meth/*)*/(j: Int = 0) = j
    }
    class O {
      def test: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ becomes
    """
    object X extends App {
      O().test.meth2()
    }
    class C {
      def /*(*/meth2/*)*/(j: Int = 0) = j
    }
    class O {
      def test: C = ???
    }
    object O {
      def apply(): O = ???
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("meth2"))

  @Test
  def testRenameWithDefaultArgs1002564Ex3() = new FileSet {
    """
    package test

    object Bug {
      class Class {
        def /*(*/renameMe/*)*/(i: Int = 42) = i
      }

      def cl = new Class
      cl.renameMe()
    }
    """ becomes
    """
    package test

    object Bug {
      class Class {
        def /*(*/ohNo/*)*/(i: Int = 42) = i
      }

      def cl = new Class
      cl.ohNo()
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("ohNo"))

  @Test
  def testRenameWithDefaultArgs1002564Ex4() = new FileSet {
    """
    object X3 {
      O3().test.meth(0)
    }

    class C3 {
      def meth(i: Int = 0, j: Int = 1) = i + j
    }

    class O3 {
      def /*(*/test/*)*/: C3 = ???
    }

    object O3 {
      def apply(): O3 = ???
    }
    """ becomes
    """
    object X3 {
      O3().test2.meth(0)
    }

    class C3 {
      def meth(i: Int = 0, j: Int = 1) = i + j
    }

    class O3 {
      def /*(*/test2/*)*/: C3 = ???
    }

    object O3 {
      def apply(): O3 = ???
    }
    """ -> TaggedAsGlobalRename
  } prepareAndApplyRefactoring(prepareAndRenameTo("test2"))

  /*
   * See Assembla Ticket 1002609
   */
  @Test
  def testRenameMemberOfPkgObjs1002609Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    package object bugs {
      def /*(*/tryRenameMe/*)*/ = "buggy"
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    package object bugs {
      def /*(*/ups/*)*/ = "buggy"
    }
    """ -> TaggedAsGlobalRename;

    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.tryRenameMe
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.ups
    }
    """
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameMemberOfPkgObjs1002609Ex2() = new FileSet {
    """
    package com.github.mlangc.experiments

    package object bugs {
      val /*(*/tryRenameMe/*)*/ = "buggy"
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    package object bugs {
      val /*(*/ups/*)*/ = "buggy"
    }
    """ -> TaggedAsGlobalRename;

    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.tryRenameMe
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.ups
    }
    """
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameMemberOfPkgObjs1002609Ex3() = new FileSet {
    """
    package com.github.mlangc.experiments

    package object bugs {
      lazy val /*(*/tryRenameMe/*)*/ = "buggy"
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    package object bugs {
      lazy val /*(*/ups/*)*/ = "buggy"
    }
    """ -> TaggedAsGlobalRename;

    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.tryRenameMe
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = bugs.ups
    }
    """
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))

  @Test
  def testRenameMemberOfNormalObjectNotAffectedBy1002609Ex1() = new FileSet {
    """
    package com.github.mlangc.experiments

    object Bugs {
      def /*(*/tryRenameMe/*)*/ = "buggy"
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    object Bugs {
      def /*(*/ups/*)*/ = "buggy"
    }
    """ -> TaggedAsGlobalRename;

    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = Bugs.tryRenameMe
    }
    """ becomes
    """
    package com.github.mlangc.experiments

    class Bug {
      val bug = Bugs.ups
    }
    """
  } prepareAndApplyRefactoring(prepareAndRenameTo("ups"))
}
