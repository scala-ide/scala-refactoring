package scala.tools.refactor.tests

import utils.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class EssentialPartitionerTest extends TestHelper {
    
  @Test
  def testSingleObject = "object A" essentialPartsAre "→0(0)❨|A|❩"
  
  @Test
  def testClassBraces = "class A { type T }" essentialPartsAre "→0(0)❨|A|→0(0)❨|type|T|❩|❩"
  
  @Test
  def testClassBraces2 = "class A { def a = { 2 } }" essentialPartsAre "→0(0)❨|A|→0(0)❨|def|a|→0(0)❨|2|❩|❩|❩"
  
  @Test
  def testClassIndentation() = {
    """ 
  class A {
    val a: Int
    def b = {
      5
    }
  }
    """ essentialPartsAre """→0(0)❨|A|→2(2)❨|a|Int|def|b|→4(2)❨|5|❩|❩|❩"""
  }  
  
  @Test
  def testMethodIndentation() = {
    """ 
  class A
    """ essentialPartsAre """→0(0)❨|A|❩"""
  }
}