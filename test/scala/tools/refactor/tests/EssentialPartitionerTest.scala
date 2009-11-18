package scala.tools.refactor.tests

import utils.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class EssentialPartitionerTest extends TestCase with TestHelper {
    
  def testSingleObject = "object A" essentialPartsAre "❨|A|❩"
  
  def testClassBraces = "class A { type T }" essentialPartsAre "❨|A|❨|type|T|❩|❩"
  
  def testClassBraces2 = "class A { def a = { 2 } }" essentialPartsAre "❨|A|❨|❨|def|a|❨|2|❩|❩|❩|❩"
}