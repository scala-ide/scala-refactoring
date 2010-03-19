package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.RenameLocal
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class RenameLocalTest extends TestHelper with TestRefactoring {
  
  def renameTo(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new RenameLocal(global) with /*Silent*/Tracing with FullIndexes {
      pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) foreach ( index.processTree _ )
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters {
      val newName = name
    })
  }.changes
  
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
      def rename(  a/*(*//*)*/  : String) {
        println(a)
      }
    }
    """,
    """
    package renameParameter
    class A {
      def rename(  b: String) {
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
      def doWithA(a: A) = new A
    }
    """,
    """
    package renameClass
    object C extends X {
      val a = new X
      def doWithA(a: X) = new X
    }
    """)   
  } applyRefactoring(renameTo("X"))
}
