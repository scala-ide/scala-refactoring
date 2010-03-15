package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.RenameLocal
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class RenameLocalTest extends TestHelper with TestRefactoring {
    
  implicit def stringToRefactoring(src: String) = new TestRefactoringImpl(src) {
    val refactoring = new RenameLocal(global) with /*Silent*/Tracing
    def rename(name: String, e: String) = doIt(e, new refactoring.RefactoringParameters {
      val newName = name
    })
  }
    
  @Test
  def renameLocalValue = """
    class A {
      def double(s: String) = s + s
      def extractFrom {
        val s = "hallo"
/*(*/   s   /*)*/ .length   
        double(s + "a")
      }
    }
    """ rename("b",
    """
    class A {
      def double(s: String) = s + s
      def extractFrom {
        val b = "hallo"
/*(*/   b   /*)*/ .length   
        double(b + "a")
      }
    }
    """)
        
  @Test
  def renameParameter = """
    class A {
      def rename(  a/*(*//*)*/  : String) {
        println(a)
      }
    }
    """ rename("b",
    """
    class A {
      def rename(  b: String) {
        println(b)
      }
    }
    """)    
    
  @Test
  def renameWithTypeAscription = """
    class A {
      def rename(a: String) {
        a match {
          case b: String => /*(*/  b  /*)*/
        }
      }
    }
    """ rename("c",
    """
    class A {
      def rename(a: String) {
        a match {
          case c: String => /*(*/  c  /*)*/
        }
      }
    }
    """)
    
  @Test
  def renameMultiAssignment = """
    class A {
      def print {
        val (/*(*/a/*)*/, b) = (5, 6)
        println(a + b)
      }
    }
    """ rename("c",
    """
    class A {
      def print {
        val (/*(*/c/*)*/, b) = (5, 6)
        println(c + b)
      }
    }
    """)
    
  @Test
  def renameBinding = """
    class A {
      def print {
        1 match { case /*(*/ i /*)*/ => i }
      }
    }
    """ rename("integer",
    """
    class A {
      def print {
        1 match { case /*(*/ integer /*)*/ => integer }
      }
    }
    """)
    
  @Test
  def renameNewVal = """
    class A(i: Int) {
      def print {
        var  /*(*/  l = /*)*/  new A(5)
      }
    }
    """ rename("ls",
    """
    class A(i: Int) {
      def print {
        var  /*(*/  ls = /*)*/  new A(5)
      }
    }
    """)
    
  @Test
  def renameLazyArg = """
    class A(i: Int) {
      def print(a: => String) {
        println(/*(*/  a  /*)*/)
      }
    }
    """ rename("s",
    """
    class A(i: Int) {
      def print(s: => String) {
        println(/*(*/  s  /*)*/)
      }
    }
    """)
        
  @Test
  def forComprehension = """
    class A {
      def print {
        for(  /*(*/  i  /*)*/  <- 1 to 10) yield i
      }
    }
    """ rename("index",
    """
    class A {
      def print {
        for(  /*(*/  index  /*)*/  <- 1 to 10) yield index
      }
    }
    """)
            
  @Test
  def inConstructor = """
    class A(i: Int) {
      val /*(*/  y  /*)*/ = i * 2
    }
    """ rename("iTimesTwo",
    """
    class A(i: Int) {
      val /*(*/  iTimesTwo  /*)*/ = i * 2
    }
    """)
    
}
