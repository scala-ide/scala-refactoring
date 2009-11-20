package scala.tools.refactor.tests

import utils.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class PartitionerTest extends TestHelper {
   
  @Test
  def testSingleObject = "object A" partitionsInto "object |A"
  
  @Test
  def testSingleClass = "class A" partitionsInto "class |A"
  
  @Test
  def testSingleAbstractClass = "abstract class A" partitionsInto "abstract| class |A"
  
  @Test
  def testSingleTrait = "trait C" partitionsInto "trait| |C"
  
  @Test
  def testClassWithManyModifiers = "final /*comment*/ class X" partitionsInto "final| /*comment*/ class |X"
  
  @Test
  def testSingleTraitWithComment = "trait /*comment*/ C" partitionsInto "trait| /*comment*/ |C"

  @Test
  def testObjectWithWS =
    """
        // here comes an object:
        private object A
    """	partitionsInto
    """
        // here comes an object:
        |private| object |A|
    """


  @Test
  def testPrettyPackages = "/**/ package /**/ x/**/./**/y/**/./**/z/**/" partitionsInto "/**/ package /**/ |x|/**/./**/|y|/**/./**/|z|/**/"
  
  @Test
  def testPackageAndClass = 
    """
        package x
        
        final class A
    """ partitionsInto
    """
        package |x|
        
        |final| class |A|
    """

  @Test
  def testClassExtends = "class X extends AnyRef" partitionsInto "class |X| extends |AnyRef"
  
  @Test
  def testClassExtendsWithTrait =
  """
    trait A; trait B
    class X extends AnyRef with A with B
  """ partitionsInto
  """
    |trait| |A|; |trait| |B|
    class |X| extends |AnyRef| with |A| with |B|
  """

  @Test
  def testClassWithBody = 
  """
    class X extends AnyRef {
      
    }
  """ partitionsInto
  """
    class |X| extends |AnyRef| {
      
    }
  """

  @Test
  def testCaseClass = "case class X(i: Int, s: String)" partitionsInto "case| class |X|(|i|: |Int|, |s|: |String|)" 
  
  @Test
  def testClassParamsWithBody =
  """
    class Xyz(private val abc: String, var int: Int) {
    }
  """ partitionsInto
  """
    class |Xyz|(|private| val |abc|: |String|, var |int|: |Int|) {
    }
  """

  @Test
  def testTraitBody = "trait A; class Xyz extends A { object C }/*done*/" partitionsInto "trait| |A|; class |Xyz| extends |A| |❨|{ object |C| }|❩|/*done*/" 
    
  @Test
  def testNestedPackages = "package x.y.z" partitionsInto "package |x|.|y|.|z"

  @Test
  def testPackage = "class Abc //done" partitionsInto "class |Abc| //done"
  
  @Test
  def testClassParams = "class Xyz(i: Int/**/)/**/" partitionsInto "class |Xyz|(|i|: |Int|/**/)/**/"
  
  @Test
  def testEarlyDef = "trait A; class Xyz extends { type T } with A {  }/*done*/" partitionsInto "trait| |A|; class |Xyz| extends { |type| |T| } with |A| {  }/*done*/"

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
  """
    |trait| |Greeting| |❨|{
      |❨|val |name|: |String|❩|
      |val| |msg| = |"How are you, "| |+|name|
    }|❩|
    class |C| extends {
      |❨|❨|{
      val |name| = |"Bob"|
    }|❩|❩|
    } with |Greeting| |❨|{
      |println|(|msg|)
    }|❩|
  """

  @Test
  def testNew = 
  """
    trait A {
      def a = new A {
        
      }
    }
  """ partitionsInto 
  """
    |trait| |A| |❨|{
      |❨|def| |a| = |new| |A| {
        
      }|❩|
    }|❩|
  """

  @Test
  def testSuper = 
  """
    class A {
      override def toString = super.toString()
    }
  """ partitionsInto 
  """
    class |A| |❨|{
      |❨|override| |def| |toString| = |❨|super|.|toString|()|❩|❩|
    }|❩|
  """

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
  """
    object |A| |❨|{
      |❨|def| |main|(|args|: |Array|[|String|]) |❨|{
        |args|.|foreach|(|println|)
        |args|.|foreach|(|println| _)
        |args|.|foreach|(|s| => |println|(|s|))
      }|❩|❩|
    }|❩|
  """

}