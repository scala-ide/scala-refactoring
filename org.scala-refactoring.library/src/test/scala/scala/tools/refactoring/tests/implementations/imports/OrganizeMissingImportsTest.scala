/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import implementations.OrganizeImports
import tests.util.TestRefactoring
import tests.util.TestHelper

import language.reflectiveCalls

class OrganizeMissingImportsTest extends TestHelper with TestRefactoring {
  outer =>

  def organize(imports: List[(String, String)])(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters(imports))
  }.changes

  @Test
  def applyall = new FileSet {
    """
      object Main {val lb = ListBuffer(1)}
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main {val lb = ListBuffer(1)}
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def parameter = new FileSet {
    """
      object Main { def method(l: ListBuffer) = "" }
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main { def method(l: ListBuffer) = "" }
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def returnValue = new FileSet {
    """
      object Main { def method(): ListBuffer = new collection.mutable.ListBuffer() }
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main { def method(): ListBuffer = new collection.mutable.ListBuffer() }
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def newInstance = new FileSet {
    """
      object Main { def method() = new ListBuffer() }
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main { def method() = new ListBuffer() }
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def newInstance2 = new FileSet {
    """
      object Main { def method() = new mutable.ListBuffer() }
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main { def method() = new mutable.ListBuffer() }
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def importFromMissingImport = new FileSet {
    """
      object Main { import ListBuffer._ }
    """ becomes
    """
      import collection.mutable.ListBuffer

      object Main { import ListBuffer._ }
    """
  } applyRefactoring organize("collection.mutable" -> "ListBuffer" :: Nil)

  @Test
  def missingSuperclass = new FileSet {
    """
      class Subclass extends LinkedList
    """ becomes
    """
      import collection.mutable.LinkedList

      class Subclass extends LinkedList
    """
  } applyRefactoring organize("collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def importRemovesUnneeded = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}
      import java.util.Map
      import scala.io.Source
      import scala.collection.mutable.ListBuffer

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
        val ll = new LinkedList
      }
    """ becomes
    """
      import java.lang.{String => S, _}
      import scala.collection.mutable.{LinkedList, ListBuffer}

      object Main {
        val s: String = ""
        val l = ListBuffer(1,2,3)
        val l2 = List(1,2,3)
        val ll = new LinkedList
      }
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def sort = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1); val ll = new LinkedList}
    """ becomes
    """
      import java.lang.Object
      import scala.collection.mutable.{LinkedList, ListBuffer}

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1); val ll = new LinkedList}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def collapse = new FileSet {
    """
      import java.lang.String
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null}
    """ becomes
    """
      import java.lang.{Object, String}
      import scala.collection.mutable.LinkedList

      object Main {val s: String = ""; var o: Object = null}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def sortAndCollapse = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.String
      import java.lang.Object

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1); val ll = new LinkedList}
    """ becomes
    """
      import java.lang.{Object, String}
      import scala.collection.mutable.{LinkedList, ListBuffer}

      object Main {val s: String = ""; var o: Object = null; val lb = ListBuffer(1); val ll = new LinkedList}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def collapseWithRename = new FileSet {
    """
      import java.lang.{String => S}
      import java.lang.{Object => Objekt}

      object Main {val s: String = ""; var o: Objekt = null; val ll = new LinkedList}
    """ becomes
    """
      import java.lang.{Object => Objekt, String => S}
      import scala.collection.mutable.LinkedList

      object Main {val s: String = ""; var o: Objekt = null; val ll = new LinkedList}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def removeOneFromMany = new FileSet {
    """
      import java.lang.{String, Math}

      object Main {val s: String = ""; val ll = new LinkedList}
    """ becomes
    """
      import java.lang.String
      import scala.collection.mutable.LinkedList

      object Main {val s: String = ""; val ll = new LinkedList}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def importAll = new FileSet {
    """
      import java.lang._
      import java.lang.String

      object Main {val ll = new LinkedList}
    """ becomes
    """
      import java.lang._
      import scala.collection.mutable.LinkedList

      object Main {val ll = new LinkedList}
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def importOnTrait = new FileSet {
    """
      package importOnTrait
      import java.lang._
      import java.lang.String

      trait A

      trait Main extends A {  val ll = new LinkedList
      }
    """ becomes
    """
      package importOnTrait

      import java.lang._
      import scala.collection.mutable.LinkedList

      trait A

      trait Main extends A {  val ll = new LinkedList
      }
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def importWithSpace = new FileSet {
    """

      import scala.collection.mutable.ListBuffer
      import java.lang.String

      object Main { val s: String = ""; val lb = ListBuffer(""); val ll = new LinkedList }
    """ becomes
    """

      import java.lang.String
      import scala.collection.mutable.{LinkedList, ListBuffer}

      object Main { val s: String = ""; val lb = ListBuffer(""); val ll = new LinkedList }
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)

  @Test
  def importAllWithRename = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}

      object Main { val s: String = ""; val ll = new LinkedList }
    """ becomes
    """
      import java.lang.{String => S, _}
      import scala.collection.mutable.LinkedList

      object Main { val s: String = ""; val ll = new LinkedList }
    """
  } applyRefactoring organize("scala.collection.mutable" -> "LinkedList" :: Nil)
}
