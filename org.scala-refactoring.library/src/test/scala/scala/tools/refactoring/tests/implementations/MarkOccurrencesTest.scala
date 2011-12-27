/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.MarkOccurrences
import tests.util.TestHelper
import org.junit.Assert._
import org.junit.Ignore

class MarkOccurrencesTest extends TestHelper {
  outer =>
    
  def markOccurrences(original: String, expected: String) {
    
    val tree = treeFrom(original)
    
    val markOccurrences = new MarkOccurrences with GlobalIndexes {
      val global = outer.global

      lazy val index = {
        val file = tree.pos.source.file
        val t = global.unitOfFile(file).body
        GlobalIndex(CompilationUnitIndex(t) :: Nil) 
      }
    }
    
    val start = original.indexOf(startPattern) + startPattern.length
    val end   = original.indexOf(endPattern)
    
    val (_, positions) = markOccurrences.occurrencesOf(tree.pos.source.file, start, end)
        
    val res = positions.foldLeft(original) {
      case (src, pos) =>
        src.substring(0, pos.start) + ("#" * (pos.end - pos.start)) + src.substring(pos.end, src.length)
    }
    
    assertEquals(expected, res)
  }
  
  @Test
  def methodCall = markOccurrences("""
      package renameRecursive
      class A {
        def length[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/length/*)*/(xs)
        }
      }
    """,
    """
      package renameRecursive
      class A {
        def ######[T](l: List[T]): Int = l match {
          case Nil => 0
          case x :: xs => 1 +  /*(*/######/*)*/(xs)
        }
      }
    """)

  @Test
  def fullySelected = markOccurrences("""
      class Abc {
          new /*(*/Abc/*)*/
      }
    """,
    """
      class ### {
          new /*(*/###/*)*/
      }
    """)

  @Test
  def typeParameter = markOccurrences("""
      class Abc {
          val xs: List[String] = "" :: Nil
          val x: /*(*/String/*)*/ = xs.head
      }
    """,
    """
      class Abc {
          val xs: List[######] = "" :: Nil
          val x: /*(*/######/*)*/ = xs.head
      }
    """)

  @Test
  def invalidSelection = markOccurrences("""
      class Abc {
          val xs: List[String] = "" :: Nil
          /*(*/val/*)*/ x: String = xs.head
      }
    """,
    """
      class Abc {
          val xs: List[String] = "" :: Nil
          /*(*/val/*)*/ x: String = xs.head
      }
    """)

  @Test
  def appliedTypeTreeParameter = markOccurrences("""
      class Abc {
          val xs: List[/*(*/String/*)*/] = "" :: Nil
          val x: String = xs.head
      }
    """,
    """
      class Abc {
          val xs: List[/*(*/######/*)*/] = "" :: Nil
          val x: ###### = xs.head
      }
    """)

  @Test
  def superConstructorArguments = markOccurrences("""
      class Base(s: String)
      class Sub(a: String) extends Base(/*(*/a/*)*/)
    """,
    """
      class Base(s: String)
      class Sub(#: String) extends Base(/*(*/#/*)*/)
    """)

  @Test
  def importValInInnerObject = markOccurrences("""
    object Outer {
      object Inner { 
        val /*(*/c/*)*/ = 2
      }
    }
    class Foo {
      import Outer.Inner.c
    }
    """,
    """
    object Outer {
      object Inner { 
        val /*(*/#/*)*/ = 2
      }
    }
    class Foo {
      import Outer.Inner.#
    }
    """)

  @Test
  def markImportedOccurrences = markOccurrences("""
    import java.io./*(*/File/*)*/
    object Whatever {
      val sep = File.pathSeparator
    }
    """,
    """
    import java.io./*(*/####/*)*/
    object Whatever {
      val sep = ####.pathSeparator
    }
    """)

  @Test
  def markImportedAndRenamedOccurrences = markOccurrences("""
    import java.io./*(*/File/*)*/
    import java.io.{File => F}
    object Whatever {
      val sep1 = File.pathSeparator
      val sep2 = F.pathSeparator
    }
    """,
    """
    import java.io./*(*/####/*)*/
    import java.io.{#########}
    object Whatever {
      val sep1 = ####.pathSeparator
      val sep2 = #.pathSeparator
    }
    """)

  @Test
  def markImportedAndRenamedOccurrencesRenamedSelected = markOccurrences("""
    import java.io.File
    import java.io.{File => F}
    object Whatever {
      val sep1 = File.pathSeparator
      val sep2 = /*(*/F/*)*/.pathSeparator
    }
    """,
    """
    import java.io.####
    import java.io.{#########}
    object Whatever {
      val sep1 = ####.pathSeparator
      val sep2 = /*(*/#/*)*/.pathSeparator
    }
    """)

  @Test
  def backtickedIdentifiers = markOccurrences("""
    trait StrangeIdentifiers {
      val /*(*/`my strange identifier`/*)*/ = "foo"
      val `my strange identifier 2` = `my strange identifier`
    }
    """,
    """
    trait StrangeIdentifiers {
      val /*(*/#######################/*)*/ = "foo"
      val `my strange identifier 2` = #######################
    }
    """)
  
  @Test
  def annotatedType = markOccurrences("""
      object U {
        def go(t: List[ /*(*/String/*)*/ ]) = {
          val s: String = ""
          t: List[String]
        }
      }
    """,
    """
      object U {
        def go(t: List[ /*(*/######/*)*/ ]) = {
          val s: ###### = ""
          t: List[######]
        }
      }
    """)
  
  @Test
  def filterInForComprehension1 = markOccurrences("""
      object U {
        for (/*(*/foo/*)*/ <- List("santa", "claus") if foo.startsWith("s")) yield foo
      }
    """,
    """
      object U {
        for (/*(*/###/*)*/ <- List("santa", "claus") if ###.startsWith("s")) yield ###
      }
    """)
  
  @Test
  def filterInForComprehension2 = markOccurrences("""
      object U {
        for (foo <- List("santa", "claus") if /*(*/foo/*)*/.startsWith("s")) yield foo
      }
    """,
    """
      object U {
        for (### <- List("santa", "claus") if /*(*/###/*)*/.startsWith("s")) yield ###
      }
    """)
  
  @Test
  def filterInForComprehension3 = markOccurrences("""
      object U {
        for (foo <- List("santa", "claus") if foo.startsWith("s")) yield /*(*/foo/*)*/
      }
    """,
    """
      object U {
        for (### <- List("santa", "claus") if ###.startsWith("s")) yield /*(*/###/*)*/
      }
    """)
  
  @Test
  def filterInForComprehension4 = markOccurrences("""
      object U {
        for (foo <- List("santa", "claus") if "".startsWith(/*(*/foo/*)*/)) yield foo
      }
    """,
    """
      object U {
        for (### <- List("santa", "claus") if "".startsWith(/*(*/###/*)*/)) yield ###
      }
    """)
  
  @Test
  def filterInForComprehensions = markOccurrences("""
      object U {
        for (/*(*/foo/*)*/ <- List("santa", "2claus"); bar <- List(1,2) if foo.startsWith(""+ bar)) yield foo
      }
    """,
    """
      object U {
        for (/*(*/###/*)*/ <- List("santa", "2claus"); bar <- List(1,2) if ###.startsWith(""+ bar)) yield ###
      }
    """)
  
  @Test
  def filterInForComprehensions2 = markOccurrences("""
      object U {
        for (foo <- List("santa", "2claus"); /*(*/bar/*)*/ <- List(1,2) if foo.startsWith(""+ bar)) yield foo
      }
    """,
    """
      object U {
        for (foo <- List("santa", "2claus"); /*(*/###/*)*/ <- List(1,2) if foo.startsWith(""+ ###)) yield foo
      }
    """)
}
