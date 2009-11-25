package scala.tools.refactor.tests

import utils.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class PartitionerTest extends TestHelper {
   
  @Test
  def testSingleObject = "object A" partitionsInto "→0(0)❨|object |A|❩"
  
  @Test
  def testSingleClass = "class A" partitionsInto "→0(0)❨|class |A|❩"
  
  @Test
  def testSingleAbstractClass = "abstract class A" partitionsInto "→0(0)❨|abstract| class |A|❩"
  
  @Test
  def testSingleTrait = "trait C" partitionsInto "→0(0)❨|trait| |C|❩"
  
  @Test
  def testClassWithManyModifiers = "final /*comment*/ class X" partitionsInto "→0(0)❨|final| /*comment*/ class |X|❩"
  
  @Test
  def testSingleTraitWithComment = "trait /*comment*/ C" partitionsInto "→0(0)❨|trait| /*comment*/ |C|❩"

  @Test
  def testObjectWithWS =
    """
        // here comes an object:
        private object A
    """	partitionsInto
    """→0(0)❨|
        // here comes an object:
        |private| object |A|
    |❩"""


  @Test
  def testPrettyPackages = "/**/ package /**/ x/**/./**/y/**/./**/z/**/" partitionsInto "→0(0)❨|/**/ package /**/ |x|/**/./**/|y|/**/./**/|z|/**/|❩"
  
  @Test
  def testPackageAndClass = 
    """
        package x
        
        final class A
    """ partitionsInto
    """→0(0)❨|
        package |x|
        
        |final| class |A|
    |❩"""

  @Test
  def testClassExtends = "class X extends AnyRef" partitionsInto "→0(0)❨|class |X| extends |AnyRef|❩"
  
  @Test
  def testClassExtendsWithTrait =
  """
    trait A; trait B
    class X extends AnyRef with A with B
  """ partitionsInto
  """→0(0)❨|
    |trait| |A|; |trait| |B|
    class |X| extends |AnyRef| with |A| with |B|
  |❩"""

  @Test
  def testClassWithBody = 
  """
    class X extends AnyRef {
      
    }
  """ partitionsInto
  """→0(0)❨|
    class |X| extends |AnyRef| |→4(4)❨|{
      
    }|❩|
  |❩"""

  @Test
  def testCaseClass = "case class X(i: Int, s: String)" partitionsInto "→0(0)❨|case| class |X|(|i|: |Int|, |s|: |String|)|❩" 
  
  @Test
  def testClassParamsWithBody =
  """
    class Xyz(private val abc: String, var int: Int) {
    }
  """ partitionsInto
  """→0(0)❨|
    class |Xyz|(|private| val |abc|: |String|, var |int|: |Int|) |→4(4)❨|{
    }|❩|
  |❩"""

  @Test
  def testTraitBody = "trait A; class Xyz extends A { object C }/*done*/" partitionsInto "→0(0)❨|trait| |A|; class |Xyz| extends |A| |→0(0)❨|{ object |C| }|❩|/*done*/|❩" 
    
  @Test
  def testNestedPackages = "package x.y.z" partitionsInto "→0(0)❨|package |x|.|y|.|z|❩"

  @Test
  def testPackage = "class Abc //done" partitionsInto "→0(0)❨|class |Abc| //done|❩"
  
  @Test
  def testClassParams = "class Xyz(i: Int/**/)/**/" partitionsInto "→0(0)❨|class |Xyz|(|i|: |Int|/**/)/**/|❩"
  
  @Test
  def testEarlyDef = "trait A; class Xyz extends { type T } with A {  }/*done*/" partitionsInto "→0(0)❨|trait| |A|; class |Xyz| extends { |type| |T| } with |A| |→0(0)❨|{  }|❩|/*done*/|❩"

  @Test
  def testEarlyDefFromSpec5_1_8 =
  """
    trait Greeting {
      val name: String
      val msg = "How are you, " +name
    }
    class C extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
  """ partitionsInto 
  """→0(0)❨|
    |trait| |Greeting| |→4(4)❨|{
      val |name|: |String|
      |val| |msg| = |"How are you, "| |+|name|
    }|❩|
    class |C| extends |→4(4)❨|{
      val |name| = |"Bob"|
    }|❩| with |Greeting| |→4(4)❨|{
      |println|(|msg|)
    }|❩|
  |❩"""

  @Test
  def testNew = 
  """
    trait A {
      def a = new A {
        
      }
    }
  """ partitionsInto 
  """→0(0)❨|
    |trait| |A| |→4(4)❨|{
      |def| |a| = |new| |A| |→6(2)❨|{
        
      }|❩|
    }|❩|
  |❩"""

  @Test
  def testSuper = 
  """
    class A {
      override def toString = super.toString()
    }
  """ partitionsInto 
  """→0(0)❨|
    class |A| |→4(4)❨|{
      |override| |def| |toString| = |→6(2)❨|super|.|toString|()|❩|
    }|❩|
  |❩"""

  @Test
  def testFunctions = 
  """
    object A {
      def main(args: Array[String]) {
        args.foreach(println)
        args.foreach(println _)
        args.foreach(s => println(s))
      }
    }
  """ partitionsInto 
  """→0(0)❨|
    object |A| |→4(4)❨|{
      |def| |main|(|args|: |Array|[|String|]) |→6(2)❨|{
        |args|.|foreach|(|println|)
        |args|.|foreach|(|println| _)
        |args|.|foreach|(|s| => |println|(|s|))
      }|❩|
    }|❩|
  |❩"""

}