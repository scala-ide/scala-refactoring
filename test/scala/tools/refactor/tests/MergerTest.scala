package scala.tools.refactor.tests

import scala.tools.refactor.tests.utils.TestTransform
import scala.tools.refactor.UnknownPosition
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.ast.parser.Tokens

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactor.printer._

@Test
class MergerTest extends TestHelper with TestTransform {

  @Test
  def testSortClassParameters() = {
    "class A(i: Int, s: String)" transformsTo ("class A(s: String, i: Int)", reverseClassParameters.transform(_))
    "class A(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int)" transformsTo ("class A(i5: Int, i4: Int, i3: Int, i2: Int, i1: Int)", reverseClassParameters.transform(_))
    "class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef" transformsTo ("class A(/*2a*/s: /*2b*/String/*2c*/, /*1a*/i:/*1b*/Int/*1c*/) extends AnyRef", reverseClassParameters.transform(_))
  }
  
  @Test
  def testSortClassParametersAndComment() = {
    """
      class A(i: Int, s: String)
      // comment 
    """ transformsTo (
    """
      class A(s: String, i: Int)
      // comment 
    """,
      reverseClassParameters.transform(_))
  }
  
  @Test
  def testSortSimpleClassMembers() = {
     """
      class A { //
        val b: String
        val c: Int = 5
      }
    """ transformsTo( 
    """
      class A { //
        val c: Int = 5
        val b: String
      }
    """, 
      reverseClassParameters.transform(_))
  }  
  
  @Test
  def testSortWithJustOne() = {
     """
      class A { //
        def a(i: Int) {
          i
        }
      }
    """ transformsTo( 
    """
      class A { //
        def a(i: Int)  = {
          i
        }
      }
    """, 
      reverseClassParameters.transform(_))
  }
  
  @Test
  def testSortClassMembersAndArguments() = {
    """
      class A(i: Int, j: Int) { //
        val b: String
        val c: Int = 5
      }
    """ transformsTo( 
    """
      class A(j: Int, i: Int) { //
        val c: Int = 5
        val b: String
      }
    """, 
      reverseClassParameters.transform(_))   
      
  }
  
  @Test
  def testSortClassMembers() = {
    """
      class A {
        val b: String
        def c: Unit = {
          5
        }
        def d: Int = {
          5
        }
      }
    """ transformsTo( 
    """
      class A {
        def d: Int = {
          5
        }
        def c: Unit = {
          5
        }
        val b: String
      }
    """, 
      reverseClassParameters.transform(_))
  }
  
  @Test
  def testSortNestedClassMembers() = {
        """
      class A {
        val b: String //b-string
        def c: Unit = {
          def d: Int = {
            5
          }
          d
        } //end of c
        
      }
    """ transformsTo( 
    """
      class A {
        def c: Unit = {
          def d: Int = {
            5
          }
          d
        } //end of c
        val b: String //b-string
        
      }
    """, 
      reverseClassParameters.transform(_))
  }
  
  @Test
  def testInsertVal() = {
            """
object A {
  /*test*/ def abc(i: Int) = {
    42
  }
  /*test2*/ def b = {
    println("hello")
    5
  }
}
    """ transformsTo( 
    """
object A {
  def method: Int = {
    555
    sample: Int
  }
  /*test*/ def abc(i: Int) = {
    42
  }
  /*test2*/ def b = {
    println("hello")
    5
  }
}
    """, 
      insertNewMethod.transform(_))
  }    
  
  @Test
  def testCopyLastMethod() = {
            """
object A {

  /*test*/ def abc(i: Int) = {
    42
  }
  /*test2*/ def b = {
    println("hello")
    5
  }
}
    """ transformsTo( 
    """
object A {
  /*test2*/ def b = {
    println("hello")
    5
  }

  /*test*/ def abc(i: Int) = {
    42
  }
  /*test2*/ def b = {
    println("hello")
    5
  }
}
    """, 
      copyLastMethod.transform(_))
  }
  
  @Test
  def testMethodFromExistingBody() = {
            """
object A {
  /*test2*/ def b = {
    println("hello, this is method b")
    5
  }
}
    """ transformsTo( 
    """
object A {
  def newMethod(arg: Int): Int = {
    println("hello, this is method b")
    5
  }
  /*test2*/ def b = {
    println("hello, this is method b")
    5
  }
}
    """, 
      newMethodFromExistingBody.transform(_))
  }  

}

