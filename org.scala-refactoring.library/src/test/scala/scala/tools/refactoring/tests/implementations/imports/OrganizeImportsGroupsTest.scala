/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import org.junit.Test

import language.reflectiveCalls

class OrganizeImportsGroupsTest extends OrganizeImportsBaseTest {
    
  def organize(groups: List[String])(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    import refactoring._
    val options = List(ExpandImports, SortImports, GroupImports(groups))
    val params = new RefactoringParameters(options = options, deps = Dependencies.FullyRecompute)
  }.mkChanges
  
  val source = """
      import scala.collection.mutable.ListBuffer
      import java.util.BitSet
      import scala.xml.Comment
      import java.util.AbstractList
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  
  @Test
  def noGrouping = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.xml.Comment

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List())
  
  @Test
  def oneScalaGroup = new FileSet {
    source becomes
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.xml.Comment

      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List("scala"))
  
  @Test
  def scalaAndJavaGroup = new FileSet {
    source becomes
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.xml.Comment

      import java.util.AbstractList
      import java.util.BitSet

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List("scala", "java"))
  
  @Test
  def severalScalaGroups = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import scala.xml.Comment

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List("java", "scala.collection", "scala.xml"))
  
  @Test
  def emptyGroups = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import org.xml.sax.Attributes
      import scala.xml.Comment

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List("java", "scala.collection", "scala.tools")) 
  
  @Test
  def packagesNeedToMatchCompletely = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.xml.Comment

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Comment)
      }
    """
  } applyRefactoring organize(List("javava", "sca"))
}
