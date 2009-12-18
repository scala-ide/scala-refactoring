package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.utils.TestTransform
import scala.tools.refactoring.UnknownPosition
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.ast.parser.Tokens

import scala.tools.refactoring.tests.utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.transformation._

@Test
class MergerTest extends TestHelper with TestTransform {

  @Test
  def testSortClassParameters() = {
    "class A(i: Int, s: String)" transformsTo ("class A(s: String, i: Int)", reverseClassParameters)
    "class A(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int)" transformsTo ("class A(i5: Int, i4: Int, i3: Int, i2: Int, i1: Int)", reverseClassParameters)
    "class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef" transformsTo ("class A(/*2a*/s: /*2b*/String/*2c*/, /*1a*/i:/*1b*/Int/*1c*/) extends AnyRef", reverseClassParameters)
  }
  
  @Test
  def testSortClassParametersAndComment() = {
    """
      // comment 
      class A(i: Int, s: String)
    """ transformsTo (
    """
      // comment 
      class A(s: String, i: Int)
    """,
      reverseClassParameters)
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
      reverseClassParameters)
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
        def a(i: Int) {
          i
        }
      }
    """, 
      reverseClassParameters)
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
      reverseClassParameters)   
      
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
      reverseClassParameters)
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
      reverseClassParameters)
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
  
  @Test
  def testbodyInBody() = {
    """
      class A {
        val b: String //b-string
        def c: Unit = {
          def d: Int = {
            5
          }
          d
        }
      }
    """ transformsTo( 
    """
      class A {
        val b: String //b-string
        def c: Unit = {
          def innerMethod: Unit = {
            def d: Int = {
              5
            }
            d
          }
          innerMethod
        }
      }
    """, 
      bodyInBody.transform(_))
  }  

}

