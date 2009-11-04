package scala.tools.refactor.tests

import utils._

import scala.tools.refactor.printer._

import junit.framework._

class SimpleTest extends TestCase with PrinterTest {
  
  def testSimpleClass = assert print "class A(/*1a*/i:/*1b*/Int/*1c*/, /*2a*/s: /*2b*/String/*2c*/) extends AnyRef"
  
  def testSingleObject = assert print "object A"
  
  def testSingleClass = assert print "class B"
  
  def testSingleAbstractClass = assert print "abstract class B"
  
  def testSingleTrait = assert print "trait C"
  
  def testClassWithManyModifiers = assert print "final /*comment*/ class X"
  
  def testSingleTraitWithComment = assert print "trait /*comment*/ C"

  def testObjectWithWS = assert print
    """
    		// here comes an object:
    		private object A
    """

  def testPackage = assert print "package x\n\n\n"

  def testPrettyPackages = assert print "/**/ package /**/ x/**/./**/y/**/./**/z/**/"
  
  def testPackageAndClass = assert print 
    """
    		package x
    		
    		final class A
    """

  def testClassExtends = assert print "class X extends AnyRef"
  
  def testClassExtendsWithTrait = assert print 
  """
    trait A; trait B
    class X extends AnyRef with A with B
  """  

  def testClassWithBody = assert print 
  """
    class X extends AnyRef {
      
    }
  """

  def testCaseClass = assert print "case class X(i: Int, s: String) extends AnyRef" 
  
  
  def testClassParamsWithBody = assert print
  """
    class Xyz(private val abc: String, var int: Int) {
    }
  """

  def testTraitBody = assert print "trait A; class Xyz extends A { object C }/*done*/"
  
  def testEarlyDef = assert print "trait A; class Xyz extends { type T } with A {  }/*done*/"
  
  def testClassParams = assert print "class Xyz(private val abc: String, var int: Int/**/)/*done!*/"
  
  def testNestedPackages = assert print "package x.y.z"
//  
//  def testEarlyDefFromSpec5_1_8 = assert print
//  """
//    trait Greeting {
//      val name: String
//      val msg = "How are you, " +name
//    }
//    class C extends {
//      val name = "Bob"
//    } with Greeting {
//      println(msg)
//    }
//  """
}