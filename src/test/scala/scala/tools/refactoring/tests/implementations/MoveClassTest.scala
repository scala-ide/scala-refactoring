package scala.tools.refactoring
package tests.implementations


import implementations.MoveClass
import tests.util.TestRefactoring
import tests.util.TestHelper


class MoveClassTest extends TestHelper with TestRefactoring {

  private def createRefactoring(pro: FileSet) = {
    new TestRefactoringImpl(pro) {
      override val refactoring = new MoveClass with TestProjectIndex
    }
  }

  private def moveTo(target: String)(pro: FileSet) = {
    val testRefactoring = createRefactoring(pro)
    import testRefactoring._
    performRefactoring(refactoring.RefactoringParameters(target, preparationResult.right.get))
  }

  @Test
  def moveBetweenPackages() = new FileSet {
    """
      package a.b.c
      class ToMove
    """ becomes
    """
      package x.y
      class ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveWithoutEmptyLineAtEOF() = new FileSet {
    """
      package a.b.c

      class X

      class /*(*/ToMove/*)*/ {
   } """ becomes
    """
      package a.b.c

      class X"""
    NewFile becomes """
      package hehe

      class /*(*/ToMove/*)*/ {
   } """
  } applyRefactoring(moveTo("hehe"))

  @Test
  def moveFromDefaultPackage() = new FileSet {
    """
      import java.util.ArrayList

      class ToMove
    """ becomes
    """
      package a.b

      class ToMove
    """
  } applyRefactoring(moveTo("a.b"))

  @Test
  def moveToParent() = new FileSet {
    """
      package a.b.c

      import java.util.ArrayList

      class ToMove
    """ becomes
    """
      package a.b

      class ToMove
    """
  } applyRefactoring(moveTo("a.b"))

  @Test
  def moveToSubpackage() = new FileSet {
    """
      package a

      class ToMove
    """ becomes
    """
      package a.b.c

      class ToMove
    """
  } applyRefactoring(moveTo("a.b.c"))

  @Test
  def moveObjectBetweenPackages() = new FileSet {
    """
      package a.b.c
      import java.util.ArrayList
      object ObjectToMove
    """ becomes
    """
      package x.y
      object ObjectToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveBetweenNestedPackages() = new FileSet {
    """
      package a
      package b
      package c
      class ToMove
    """ becomes
    """
      package x.y
      class ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveBetweenSubPackage() = new FileSet {
    """
      package org.com
      package pkg
      class ToMove
    """ becomes
    """
      package org.com
      package other
      class ToMove
    """
  } applyRefactoring(moveTo("org.com.other"))

  @Test
  def moveToSuperPackage() = new FileSet {
    """
      package org.com
      package pkg
      class ToMove
    """ becomes
    """
      package org.com
      class ToMove
    """
  } applyRefactoring(moveTo("org.com"))

  @Test
  def moveOneFromManyWithSelfReference() = new FileSet {
    """
      package org.com
      package pkg

      class /*(*/ToMove/*)*/ {
        var x: ToMove = _
      }

      class SomeOtherClass
    """ becomes
    """
      package org.com
      package pkg

      class SomeOtherClass
    """
    ;
    NewFile becomes """
      package org.com

      class /*(*/ToMove/*)*/ {
        var x: ToMove = _
      }
    """
  } applyRefactoring(moveTo("org.com"))

  @Test
  def moveOneFromMany() = new FileSet {
    """
      package org.com
      package pkg

      class /*(*/ToMove/*)*/

      class SomeOtherClass
    """ becomes
    """
      package org.com
      package pkg

      class SomeOtherClass
    """
    ;
    NewFile becomes """
      package org.com

      class /*(*/ToMove/*)*/
    """
  } applyRefactoring(moveTo("org.com"))

  @Test
  def moveOneFromManyWithDoc() = new FileSet {
    """
      package org.com
      package pkg

      /*
       * Here's some documentation that should be moved along.
       **/
      class /*(*/ToMove/*)*/

      /*
       * Documentation that stays here.
       **/
      class SomeOtherClass
    """ becomes
    """
      package org.com
      package pkg

      /*
       * Documentation that stays here.
       **/
      class SomeOtherClass
    """
    ;
    NewFile becomes """
      package org.com
      package whatever

      /*
       * Here's some documentation that should be moved along.
       **/
      class /*(*/ToMove/*)*/
    """
  } applyRefactoring(moveTo("org.com.whatever"))

  @Test
  def moveClassWithImplementation() = new FileSet {
    """
      package org.com
      package pkg

      import collection.mutable._

      trait /*(*/ToMove/*)*/ {
        def someMethod = 42
      }

      object SomeObj {
        var x: ToMove = null
      }
    """ becomes
    """
      package org.com
      package pkg

      import collection.mutable._
      import ch.misto.ToMove

      object SomeObj {
        var x: ToMove = null
      }
    """
    ;
    NewFile becomes """
      package ch.misto

      trait /*(*/ToMove/*)*/ {
        def someMethod = 42
      }
    """
  } applyRefactoring(moveTo("ch.misto"))

  @Test
  def moveClassesWithImports() = new FileSet {
    """
      package org.com
      package pkg

      import scala.collection.mutable.ListBuffer

      class ToMove {
        var lb: ListBuffer[Int] = _
      }

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """ becomes
    """
      package ch.misto

      import scala.collection.mutable.ListBuffer

      class ToMove {
        var lb: ListBuffer[Int] = _
      }

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """
  } applyRefactoring(moveTo("ch.misto"))

  @Test
  def wildcardsNotExpanded() = new FileSet {
    """
      package org.com
      package pkg

      import scala.collection.mutable._

      class ToMove {
        var lb: ListBuffer[Int] = _
      }
    """ becomes
    """
      package ch.misto

      import scala.collection.mutable._

      class ToMove {
        var lb: ListBuffer[Int] = _
      }
    """
  } applyRefactoring(moveTo("ch.misto"))

  @Test
  def moveClassWithImports() = new FileSet {
    """
      package org.com
      package pkg

      import scala.collection.mutable.ListBuffer

      class /*(*/ToMove/*)*/ {
        var lb: ListBuffer[Int] = _
      }

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """ becomes
    """
      package org.com
      package pkg

      import scala.collection.mutable.ListBuffer

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """
    ;
    NewFile becomes """
      package ch.misto

      import scala.collection.mutable.ListBuffer

      class /*(*/ToMove/*)*/ {
        var lb: ListBuffer[Int] = _
      }
    """
  } applyRefactoring(moveTo("ch.misto"))

  @Test
  def moveClassThatExtendsFromRequiredImport() = new FileSet {
    """
      package org.com
      package pkg

      import scala.collection.mutable.ListBuffer
      import java.util.Comparator

      class /*(*/AlwaysEquals/*)*/ extends Comparator[Int] {
        def compare(i1: Int, i2: Int) = 0
      }

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """ becomes
    """
      package org.com
      package pkg

      import scala.collection.mutable.ListBuffer
      import java.util.Comparator

      object SomeObj {
        var lb: ListBuffer[Int] = _
      }
    """
    ;
    NewFile becomes """
      package ch.misto

      import java.util.Comparator

      class /*(*/AlwaysEquals/*)*/ extends Comparator[Int] {
        def compare(i1: Int, i2: Int) = 0
      }
    """
  } applyRefactoring(moveTo("ch.misto"))

  @Test
  def moveClassWithDepOnCurrentPackage() = new FileSet {
    """
      package org.com
      package pkg

      trait A

      trait /*(*/B/*)*/ extends A
    """ becomes
    """
      package org.com
      package pkg

      trait A
    """
    ;
    NewFile becomes """
      package org.com
      package other

      import org.com.pkg.A

      trait /*(*/B/*)*/ extends A
    """
  } applyRefactoring(moveTo("org.com.other"))

  @Test
  def copyrightHeaderIsMovedAlong() = new FileSet {
    """
      // Copyright 2012 ..
      package org.com
      package pkg

      trait A

      class /*(*/B/*)*/(a: A)
    """ becomes
    """
      // Copyright 2012 ..
      package org.com
      package pkg

      trait A
    """
    ;
    NewFile becomes """
      // Copyright 2012 ..
      package ch.hsr

      import org.com.pkg.A

      class /*(*/B/*)*/(a: A)
    """
  } applyRefactoring(moveTo("ch.hsr"))

  @Test
  def moveWithAdaptedImport() = new FileSet {
    """
      package a.b.c

      class MoveIt
    """ becomes
    """
      package x.y

      class MoveIt
    """
    ;
    """
      package m.n

      import a.b.c.MoveIt

      class User extends MoveIt
    """ becomes
    """
      package m.n

      import x.y.MoveIt

      class User extends MoveIt
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveMulipleClassesWithAdaptedImport() = new FileSet {
    """
      package a.b.c

      class MoveIt
      trait MoveAlso
    """ becomes
    """
      package x.y

      class MoveIt
      trait MoveAlso
    """
    ;
    """
      package m.n

      import a.b.c.MoveIt
      import a.b.c.MoveAlso

      class User extends MoveIt with MoveAlso
    """ becomes
    """
      package m.n

      import x.y.MoveIt
      import x.y.MoveAlso

      class User extends MoveIt with MoveAlso
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def needImportToOriginatingPackage() = new FileSet {
    addToCompiler("X", """
      package a.b.c

      trait X
    """)
    ;
    """
      package a.b.c

      trait /*(*/ToMove/*)*/ {
        self: X =>
      }
    """ becomes
    """
      package x.y

      import a.b.c.X

      trait /*(*/ToMove/*)*/ {
        self: X =>
      }
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveWithAdaptedImportWithManySelectors() = new FileSet {
    addToCompiler("X", """
      package a.b.c

      trait X
    """)
    ;
    """
      package a.b.c

      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y

      class /*(*/ToMove/*)*/
    """
    ;
    """
      package m.n

      import a.b.c.{ToMove, X}

      class User extends ToMove
    """ becomes
    """
      package m.n

      import a.b.c.{X}
      import x.y.ToMove

      class User extends ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveClassWithMultipleDependenciesOnCurrentFile() = new FileSet {
    """
    package arith

    sealed abstract class Term
    case object TmZero extends Term

    class /*(*/ArithParser/*)*/ {
      def x: Unit = {
        TmZero
      }
    }
    """ becomes
    """
    package arith

    sealed abstract class Term
    case object TmZero extends Term
    """
    ;
    NewFile becomes """
    package a.b.c.d

    import arith.TmZero

    class /*(*/ArithParser/*)*/ {
      def x: Unit = {
        TmZero
      }
    }
    """
  } applyRefactoring(moveTo("a.b.c.d"))

  @Test
  def adaptFQN() = new FileSet {
    """
      package a.b.c
      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y
      class /*(*/ToMove/*)*/
    """
    ;
    """
      package m.n
      class User extends a.b.c.ToMove
    """ becomes
    """
      package m.n
      class User extends x.y.ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def adaptReferenceFromSamePackage() = new FileSet {
    """
      package a.b.c
      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y
      class /*(*/ToMove/*)*/
    """
    ;
    """
      package a.b.c

      class User extends ToMove
    """ becomes
    """
      package a.b.c

      import x.y.ToMove
      class User extends ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def adaptReferenceInValDefWithoutImport() = new FileSet {
    """
      package a.b.c
      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y
      class /*(*/ToMove/*)*/
    """
    ;
    """
      package a.b.d

      import a.b.c._

      class User(x: ToMove)
    """ becomes
    """
      package a.b.d

      import a.b.c._
      import x.y.ToMove

      class User(x: ToMove)
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def adaptReferenceInValDefWithImport() = new FileSet {
    """
      package a.b.c
      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y
      class /*(*/ToMove/*)*/
    """
    ;
    """
      package a.b.d
      import a.b.c.ToMove
      class User(x: ToMove)
    """ becomes
    """
      package a.b.d
      import x.y.ToMove
      class User(x: ToMove)
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def adaptReferenceFromSubPackage() = new FileSet {
    """
      package a.b
      package c
      class /*(*/ToMove/*)*/
    """ becomes
    """
      package x.y
      class /*(*/ToMove/*)*/
    """
    ;
    """
      package a.b
      package d
      class User extends c.ToMove
    """ becomes
    """
      package a.b
      package d
      class User extends x.y.ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveMultipleClassesWithInterdependencies() = new FileSet {
    """
    package org.scala-refactoring-library

    class A {
      val B = new {
        val y = 2
      }
    }

    object C {
      def m(x: A): Unit = {
        import x._
        println(B.y)
      }
    }
    """ becomes
    """
    package org.scala-refactoring

    class A {
      val B = new {
        val y = 2
      }
    }

    object C {
      def m(x: A): Unit = {
        import x._
        println(B.y)
      }
    }
    """
  } applyRefactoring(moveTo("org.scala-refactoring"))

  @Test
  def moveToDefaultPackage() = new FileSet {
    """
    package ctes

    object Ctes {
      val A = 2
      val B = 3
    }
    """ becomes
    """
    ▒

    object Ctes {
      val A = 2
      val B = 3
    }
    """
  } applyRefactoring(moveTo(""))

  @Test
  def moveCompanionObjectAlongNoOtherImpls() = new FileSet {
    """
    object /*(*/BFF/*)*/
    class BFF
    """ becomes
    """
    package bff

    object /*(*/BFF/*)*/
    class BFF
    """
  } applyRefactoring(moveTo("bff"))

  @Ignore
  @Test
  def moveCompanionObjectAlong() = new FileSet {
    """
    object /*(*/BFF/*)*/
    class BFF
    class Outsider
    """ becomes
    """
    class Outsider
    """
    NewFile becomes """""""
  } applyRefactoring(moveTo("bff"))

  @Test
  def moveFromDefaultPackageNoImports() = new FileSet {
    """
    object Ctes {
      val A = 2
      val B = 3
    }
    """ becomes
    """
    package ctes

    object Ctes {
      val A = 2
      val B = 3
    }
    """
  } applyRefactoring(moveTo("ctes"))

  @Test
  def updateReferencesToMovedObject() = new FileSet {
    """
    object Bar64 {
      val instance = new Bar64
    }

    class Bar64
    """ becomes
    """
    package bar

    object Bar64 {
      val instance = new Bar64
    }

    class Bar64
    """
    """
    class Foo64 {
      import Bar64.instance

      def foo = instance.toString
      def bar = Bar64.instance.toString
    }
    """ becomes """
    import bar.Bar64
    class Foo64 {
      import Bar64.instance

      def foo = instance.toString
      def bar = Bar64.instance.toString
    }
    """
  } applyRefactoring(moveTo("bar"))

  @Test
  def nestedPackageAndImports() = new FileSet(expectCompilingCode = false) {
    """
    package x
    package y

    import scala.collection.mutable.ListBuffer

    class ToMove extends Dependency {
      val l = ListBuffer[String]()
    }""" becomes
    """
    package x
    package z

    import scala.collection.mutable.ListBuffer

    class ToMove extends Dependency {
      val l = ListBuffer[String]()
    }"""
    ;
    """
      package x.y
      class Dependency
    """ becomes
    """
      package x.y
      class Dependency
    """
  } applyRefactoring(moveTo("x.z"))

  @Test
  def adaptLotsOfReferences() = new FileSet {
    """
      package a.b
      package c

      class /*(*/ToMove/*)*/ {
      }

      trait Xy {
        val other: ToMove
      }
    """ becomes
    """
      package a.b
      package c

      import x.y.ToMove
      trait Xy {
        val other: ToMove
      }
    """
    ;
    NewFile becomes
    """
      package x.y

      class /*(*/ToMove/*)*/ {
      }
    """
    ;
    """
      package different

      import a.b.c._

      class User(what: ToMove) extends ToMove
    """ becomes
    """
      package different

      import a.b.c._
      import x.y.ToMove

      class User(what: ToMove) extends ToMove
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveManyClassesAdaptReferences() = new FileSet {
    """
      package a.b.c
      trait Aa
      trait Bb
      trait Cc
    """ becomes
    """
      package x.y
      trait Aa
      trait Bb
      trait Cc
    """
    ;
    """
      package different

      import a.b.c._

      class User(what: ToMove) extends Aa with Bb
    """ becomes
    """
      package different

      import a.b.c._
      import x.y.Aa
      import x.y.Bb

      class User(what: ToMove) extends Aa with Bb
    """
    ;
    """
      package third

      import a.b.c.Cc

      class X extends Cc with a.b.c.Bb
    """ becomes
    """
      package third

      import x.y.Cc

      class X extends Cc with x.y.Bb
    """
  } applyRefactoring(moveTo("x.y"))

  @Test
  def moveObject() = new FileSet {
    """package arith

sealed abstract class Term

case object TmTrue extends Term
case object TmFalse extends Term

object /*(*/Arith/*)*/ {

  def isVal(t: Term) : Boolean = t match {
    case TmTrue | TmFalse => true
    case _ => false
  }
}
    """ becomes
    """package arith

sealed abstract class Term

case object TmTrue extends Term
case object TmFalse extends Term
    """
    NewFile becomes """package x.y

import arith.Term
import arith.TmFalse
import arith.TmTrue

object /*(*/Arith/*)*/ {

  def isVal(t: Term) : Boolean = t match {
    case TmTrue | TmFalse => true
    case _ => false
  }
}
    """
  } applyRefactoring(moveTo("x.y"))

  /*
   * See Assembla Ticket 1002511
   */
  @Test
  def moveWithUnqualifiedObjectImportAndSpecialChars() = new FileSet {
    """
    package p1

    object O {
      def ? : String = ???
    }
    """ isNotModified();
    """
    package p1

    class /*(*/C/*)*/ {
      import O._

      def f = ?
    }
    """ becomes
    """
    package p1.p2

    import p1.O

    class /*(*/C/*)*/ {
      import O._

      def f = ?
    }
    """
  } applyRefactoring(moveTo("p1.p2"))

  /*
   * See Assembla Ticket 1002511
   */
  @Test
  def moveWithQualifiedObjectImportAndSpecialChars() = new FileSet {
    """
    package p1

    object O {
      def ? : String = ???
    }
    """ isNotModified();
    """
    package p1

    class /*(*/C/*)*/ {
      def f = O.?
    }
    """ becomes
    """
    package p1.p2

    import p1.O

    class /*(*/C/*)*/ {
      def f = O.?
    }
    """
  } applyRefactoring(moveTo("p1.p2"))

  /*
   * See Assembla Ticket 1002511
   */
  @Test
  def moveWithUnqualifiedObjectImport() = new FileSet {
    """
    package p1

    object O {
      def a : String = ???
    }
    """ isNotModified();
    """
    package p1

    class /*(*/C/*)*/ {
      import O._

      def f = a
    }
    """ becomes
    """
    package p1.p2

    import p1.O

    class /*(*/C/*)*/ {
      import O._

      def f = a
    }
    """
  } applyRefactoring(moveTo("p1.p2"))

  /*
   * See Assembla Ticket 1002512
   */
  @Test
  def moveWithLocalImplicitImport() = new FileSet {
    """
    package p1

    object O {
      implicit def toX(s: String): X = ???
    }

    class X
    """ isNotModified();
    """
    package p1

    class /*(*/C/*)*/ {

      import O._

      def f: X = ""
    }
    """ becomes
    """
    package p1.p2

    import p1.O
    import p1.X

    class /*(*/C/*)*/ {

      import O._

      def f: X = ""
    }
    """
  } applyRefactoring(moveTo("p1.p2"))
}
