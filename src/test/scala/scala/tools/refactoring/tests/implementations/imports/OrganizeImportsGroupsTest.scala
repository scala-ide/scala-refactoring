/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations.imports

import scala.tools.refactoring.implementations.OrganizeImports


class OrganizeImportsGroupsTest extends OrganizeImportsBaseTest {

  def organize(groups: List[String])(pro: FileSet) = new OrganizeImportsRefatoring(pro) {
    import refactoring._
    val config = OrganizeImports.OrganizeImportsConfig(
      importsStrategy = Some(OrganizeImports.ImportsStrategy.ExpandImports),
      groups = groups)
    val params = new RefactoringParameters(deps = Dependencies.FullyRecompute, config = Some(config))
  }.mkChanges

  val source = """
      import scala.collection.mutable.ListBuffer
      import java.util.BitSet
      import scala.io.Source
      import java.util.AbstractList
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """

  @Test
  def noGrouping() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List())

  @Test
  def oneScalaGroup() = new FileSet {
    source becomes
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("scala"))

  @Test
  def scalaAndJavaGroup() = new FileSet {
    source becomes
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("scala", "java"))

  @Test
  def severalScalaGroups() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import scala.io.Source

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("java", "scala.collection", "scala.io"))

  @Test
  def emptyGroups() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import org.xml.sax.Attributes
      import scala.io.Source

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("java", "scala.collection", "scala.tools"))

  @Test
  def packagesNeedToMatchCompletely() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("javava", "sca"))

  @Test
  def defaultGroupInTheMiddleVersion1() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import java.util.AbstractList
      import java.util.BitSet
      import scala.io.Source

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("org.xml", "*", "scala.collection"))

  @Test
  def defaultGroupInTheMiddleVersion2() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import java.util.AbstractList
      import java.util.BitSet

      import scala.io.Source

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("org.xml", "java", "*", "scala.collection"))

  @Test
  def defaultGroupInTheMiddleButEmpty() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import java.util.AbstractList
      import java.util.BitSet

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("org", "java", "*", "scala"))

  @Test
  def defaultGroupInTheBeginningVersion1() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import scala.io.Source

      import org.xml.sax.Attributes

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("*", "org.xml", "scala.collection"))

  @Test
  def defaultGroupInTheBeginningVersion2() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("*", "scala", "java"))

  @Test
  def defaultGroupInTheBeginningButEmpty() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("*", "org", "scala", "java"))

  @Test
  def noGroupingIsDefaultGrouping() = new FileSet {
    source becomes
    """
      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("*"))

  @Test
  def defaultGroupInTheEndVersion1() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("org", "scala", "*"))

  @Test
  def defaultGroupInTheEndVersion2() = new FileSet {
    source becomes
    """
      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("scala", "java", "*"))

  @Test
  def defaultGroupInTheEndButEmpty() = new FileSet {
    source becomes
    """
      import org.xml.sax.Attributes

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer
      import scala.io.Source

      import java.util.AbstractList
      import java.util.BitSet

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("org", "scala", "java", "*"))

  @Test
  def mostSpecificTakesPrecedence() = new FileSet {
    source becomes
    """
      import scala.io.Source

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("scala", "scala.collection"))

  @Test
  def fromSpecificToGeneral() = new FileSet {
    source becomes
    """
      import scala.io.Source

      import scala.collection.mutable.HashMap
      import scala.collection.mutable.ListBuffer

      import java.util.AbstractList
      import java.util.BitSet
      import org.xml.sax.Attributes

      trait Temp {
        // we need some code that use the imports
        val x: (ListBuffer[Int], HashMap[String, Int])
        val y: (AbstractList[Int], BitSet)
        val z: (Attributes, Source)
      }
    """
  } applyRefactoring organize(List("scala.io", "scala"))
}
