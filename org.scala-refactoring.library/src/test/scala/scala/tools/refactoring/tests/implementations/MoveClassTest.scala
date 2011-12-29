package scala.tools.refactoring
package tests.implementations

import org.junit.Test

import implementations.MoveClass
import tests.util.{TestRefactoring, TestHelper}

class MoveClassTest extends TestHelper with TestRefactoring {
    
  private def createRefactoring(pro: FileSet) = {
    new TestRefactoringImpl(pro) {
      val refactoring = new MoveClass with ConsoleTracing with TestProjectIndex
    }
  }

  private def moveFileTo(target: String)(pro: FileSet) = {
    val testRefactoring = createRefactoring(pro)
    testRefactoring.performRefactoring(testRefactoring.refactoring.KeepFile(target))
  }

  private def moveToNewFile(target: String)(pro: FileSet) = {
    val testRefactoring = createRefactoring(pro)
    testRefactoring.performRefactoring(testRefactoring.refactoring.NewFile(target))
  }
  
  @Test
  def moveBetweenPackages = new FileSet {
    """
      package a.b.c
      class ToMove
    """ becomes
    """
      package x.y
      class ToMove
    """
  } applyRefactoring(moveFileTo("x.y"))
  
  @Test
  def moveObjectBetweenPackages = new FileSet {
    """
      package a.b.c
      object ObjectToMove
    """ becomes
    """
      package x.y
      object ObjectToMove
    """
  } applyRefactoring(moveFileTo("x.y"))
  
  @Test
  def moveBetweenNestedPackages = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))
  
  @Test
  def moveBetweenSubPackage = new FileSet {
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
  } applyRefactoring(moveFileTo("org.com.other"))
  
  @Test
  def moveToSuperPackage = new FileSet {
    """
      package org.com
      package pkg
      class ToMove
    """ becomes
    """
      package org.com
      class ToMove
    """
  } applyRefactoring(moveFileTo("org.com"))

  @Test
  def moveOneFromMany = new FileSet {
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
    "" becomes """
      package org.com

      class /*(*/ToMove/*)*/
    """
  } applyRefactoring(moveToNewFile("org.com"))

  @Test
  def moveOneFromManyWithDoc = new FileSet {
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
    "" becomes """
      package org.com
      package whatever

      /*
       * Here's some documentation that should be moved along.
       **/
      class /*(*/ToMove/*)*/
    """
  } applyRefactoring(moveToNewFile("org.com.whatever"))

  @Test
  def moveClassWithImplementation = new FileSet {
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
    "" becomes """
      package ch.misto

      trait /*(*/ToMove/*)*/ {
        def someMethod = 42
      }
    """
  } applyRefactoring(moveToNewFile("ch.misto"))
  
  @Test
  def moveClassWithImports = new FileSet {
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
    "" becomes """
      package ch.misto

      import scala.collection.mutable.ListBuffer

      class /*(*/ToMove/*)*/ {
        var lb: ListBuffer[Int] = _
      }
    """
  } applyRefactoring(moveToNewFile("ch.misto"))
  
  @Test
  def moveClassThatExtendsFromRequiredImport = new FileSet {
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
    "" becomes """
      package ch.misto

      import java.util.Comparator

      class /*(*/AlwaysEquals/*)*/ extends Comparator[Int] {
        def compare(i1: Int, i2: Int) = 0
      }
    """
  } applyRefactoring(moveToNewFile("ch.misto"))
  
  @Test
  def moveClassWithDepOnCurrentPackage = new FileSet {
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
    "" becomes """
      package org.com
      package other

      import org.com.pkg.A

      trait /*(*/B/*)*/ extends A
    """
  } applyRefactoring(moveToNewFile("org.com.other"))
  
  @Test
  def copyrightHeaderIsMovedAlong = new FileSet {
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
    "" becomes """
      // Copyright 2012 ..
      package ch.hsr

      import org.com.pkg.A

      class /*(*/B/*)*/(a: A)
    """
  } applyRefactoring(moveToNewFile("ch.hsr"))

  @Test
  def moveWithAdaptedImport = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def moveWithAdaptedImportWithManySelectors = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptFQN = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptReferenceFromSamePackage = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptReferenceInValDefWithoutImport = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptReferenceInValDefWithImport = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptReferenceFromSubPackage = new FileSet {
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
  } applyRefactoring(moveFileTo("x.y"))

  @Test
  def adaptLotsOfReferences = new FileSet {
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
    "" becomes
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
  } applyRefactoring(moveToNewFile("x.y"))
}
