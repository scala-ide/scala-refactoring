package scala.tools.refactor.tests

import utils._

import scala.tools.refactor.printer._

import junit.framework._

class SimpleTest extends TestCase with PrinterTest {
  
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

  def testNestedPackages = assert print "package x.y.z"
  
  def testPrettyPackages = assert print "/**/ package /**/ x/**/./**/y/**/./**/z/**/"
  
  def testPackageAndClass = assert print 
    """
    		package x
    		
    		final class A
    """

  def testClassExtends = assert print "class X extends AnyRef"
  
  def testClassExtendsWithTrait = assert print 
  """
    trait A
    class X extends AnyRef with A
  """
}