package scala.tools.refactoring.tests

import scala.tools.refactoring.tests.util.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class EssentialPartitionerTest extends TestHelper {
    
  @Test
  def testSingleObject = "object A" essentialFragmentsAre "→0(0)❨|A|❩"
  
  @Test
  def testClassBraces = "class A { type T }" essentialFragmentsAre "→0(0)❨|A|→0(0)❨|type|T|❩|❩"
  
  @Test
  def testClassBraces2 = "class A { def a = { 2 } }" essentialFragmentsAre "→0(0)❨|A|→0(0)❨|def|a|→0(0)❨|2|❩|❩|❩"
  
  @Test
  def testClassIndentation() = {
    """ 
  class A {
    val a: Int
    def b = {
      5
    }
  }
    """ essentialFragmentsAre """→0(0)❨|A|→2(2)❨|a|Int|def|b|→4(2)❨|5|❩|❩|❩"""
  }  
  
  @Test
  def testMethodIndentation() = {
    """ 
  class A
    """ essentialFragmentsAre """→0(0)❨|A|❩"""
  }
}