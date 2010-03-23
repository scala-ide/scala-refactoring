package scala.tools.refactoring.tests.regeneration

import scala.tools.refactoring.tests.util.TestHelper
import junit.framework.TestCase
import org.junit.Test

@Test
class PartitionerTest extends TestHelper {
  
  @Test
  def packageWithRoot = "import _root_.java.util.concurrent._" partitionsInto "→0(0)❨|import |_root_|.|java|.|util|.|concurrent|.|_|❩"
   
  @Test
  def testSingleObject = "object A" partitionsInto "→0(0)❨|object |A|❩"
  
  @Test
  def testSingleClass = "class A2" partitionsInto "→0(0)❨|class |A2|❩"
  
  @Test
  def testSingleAbstractClass = "abstract class A3" partitionsInto "→0(0)❨|abstract| class |A3|❩"
  
  @Test
  def testSingleTrait = "trait C1" partitionsInto "→0(0)❨|trait| |C1|❩"
  
  @Test
  def testClassWithManyModifiers = "final /*comment*/ class X2" partitionsInto "→0(0)❨|final| /*comment*/ class |X2|❩"
  
  @Test
  def testSingleTraitWithComment = "trait /*comment*/ C2" partitionsInto "→0(0)❨|trait| /*comment*/ |C2|❩"

  @Test
  def testObjectWithWS =
    """
        // here comes an object:
        private object A4
    """	partitionsInto
    """→0(0)❨|
        // here comes an object:
        |→8(8)❨|private| object |A4|❩|
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
        
        |→8(8)❨|final| class |A|❩|
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
    |→4(4)❨|trait| |A|❩|; |→4(4)❨|trait| |B|❩|
    |→4(4)❨|class |X| extends |AnyRef| with |A| with |B|❩|
  |❩"""

  @Test
  def testClassWithBody = 
  """
    class W extends AnyRef {
      
    }
  """ partitionsInto
  """→0(0)❨|
    |→4(4)❨|class |W| extends |AnyRef| {
      
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
    |→4(4)❨|class |Xyz|(|private| val |abc|: |String|, var |int|: |Int|) {
    }|❩|
  |❩"""

  @Test
  def testTraitBody = "trait A; class Xyz extends A { object C }/*done*/" partitionsInto "→0(0)❨|→0(0)❨|trait| |A|❩|; |→0(0)❨|class |Xyz| extends |A| |→0(0)❨|{ |→0(0)❨|object |C|❩| }|❩|❩|/*done*/|❩" 
    
  @Test
  def testNestedPackages = "package x.y.z //done" partitionsInto "→0(0)❨|package |x|.|y|.|z| //done|❩"

  @Test
  def testPackage = "class Abc //done" partitionsInto "→0(0)❨|→0(0)❨|class |Abc|❩| //done|❩"
  
  @Test
  def testClassParams = "class Xyz(i: Int/**/)/**/" partitionsInto "→0(0)❨|→0(0)❨|class |Xyz|(|i|: |Int|/**/)|❩|/**/|❩"
  
  @Test
  def testEarlyDef = "trait A; class Xyz extends { type T } with A {  }/*done*/" partitionsInto "→0(0)❨|→0(0)❨|trait| |A|❩|; |→0(0)❨|class |Xyz| extends { |type| |T| } with |A| {  }|❩|/*done*/|❩"

  @Test
  def testEarlyDefFromSpec5_1_8 =
  """
    trait Greeting {
      val name: String = "Mirko"
      val msg = "How are you, " +name
    }
    class C extends {
      val name = "Bob"
    } with Greeting {
      println(msg)
    }
  """ partitionsInto 
  """→0(0)❨|
    |→4(4)❨|trait| |Greeting| |→4(0)❨|{
      |val| |name|: |String| = |"Mirko"|
      |val| |msg| = |"How are you, "| |+|name|
    }|❩|❩|
    |→4(4)❨|class |C| extends |→4(0)❨|{
      val |name| = |"Bob"|
    }|❩| with |Greeting| |→4(0)❨|{
      |println|→6(2)❨|(|msg|)|❩|
    }|❩|❩|
  |❩"""

  @Test
  def testNew = 
  """
    trait A {
      def a = new A {
        val i = 1
      }
    }
  """ partitionsInto 
  """→0(0)❨|
    |→4(4)❨|trait| |A| |→4(0)❨|{
      |def| |a| = |new| |A| |→6(2)❨|{
        |val| |i| = |1|
      }|❩|
    }|❩|❩|
  |❩"""

  @Test
  def testSuper = 
  """
    class A {
      override def toString = super.toString()
    }
  """ partitionsInto 
  """→0(0)❨|
    |→4(4)❨|class |A| |→4(0)❨|{
      |override| |def| |toString| = |→6(2)❨|super|.|toString|()|❩|
    }|❩|❩|
  |❩"""

  @Test
  def testFunctions = 
  """
    object B {
      def main(args: Array[String]) {
        args.foreach(println)
        args.foreach(println _)
        args.foreach(s => println(s))
      }
    }
  """ partitionsInto 
  """→0(0)❨|
    |→4(4)❨|object |B| |→4(0)❨|{
      |def| |main|(|args|: |Array|[|String|]) |→6(2)❨|{
        |args|.|foreach|→8(2)❨|(|println|)|❩|
        |args|.|foreach|→8(2)❨|(|println| _)|❩|
        |args|.|foreach|→8(2)❨|(|s| => |println|→8(0)❨|(|s|))|❩|❩|
      }|❩|
    }|❩|❩|
  |❩"""
  
  @Test
  def blockAtTheEnd =
  """
      class A {
        def get(i: Int): Unit = {
          val a = 1
          a
        }
      }
  """ partitionsInto 
  """→0(0)❨|
      |→6(6)❨|class |A| |→6(0)❨|{
        |def| |get|(|i|: |Int|): |Unit| = |→8(2)❨|{
          |val| |a| = |1|
          |→10(2)❨|a|❩|
        }|❩|
      }|❩|❩|
  |❩"""  
  
  @Test
  def expressionAtEnd =
  """
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }
  """ partitionsInto 
  """→0(0)❨|
    |→4(4)❨|class |A| |→4(0)❨|{
      |def| |extractFrom|(): |Int| = |→6(2)❨|{
        |val| |a| = |1|
/*(*/   |a| |+| |1|    /*)*/
      }|❩|
    }|❩|❩|
  |❩"""

  @Test
  def applySymbol =
  """
      class A {
        def get {
          val inc: Int => Int = _ + 1
          inc(1)
        }
      }
  """ partitionsInto
  """→0(0)❨|
      |→6(6)❨|class |A| |→6(0)❨|{
        |def| |get| |→8(2)❨|{
          |val| |inc|: |Int| => |Int| = _ |+| |1|
          |→10(2)❨|inc|→10(0)❨|(|1|)|❩|❩|
        }|❩|
      }|❩|❩|
  |❩"""
  
  @Test
  def ifCondition = 
  """
    class A {
      def extractFrom {
        val abcd = false
        if(!abcd) true
      }
    }
  """ partitionsInto
  """→0(0)❨|
    |→4(4)❨|class |A| |→4(0)❨|{
      |def| |extractFrom| |→6(2)❨|{
        |val| |abcd| = |false|
        if|→8(2)❨|(|!|abcd|)|❩| |→8(2)❨|true|❩|
      }|❩|
    }|❩|❩|
  |❩"""
  
  @Test
  def newInstance = 
  """class A {
    val a = new A
  }
  """ partitionsInto
  """→0(0)❨|→0(0)❨|class |A| |→0(0)❨|{
    |val| |a| = |new| |A|
  }|❩|❩|
  |❩"""
  
  //@Test
  def superConstructor = 
  """class A(s: String)
     class B(s: String) extends A(s)
  """ partitionsInto
  """"""
  
  @Test
  def matching = 
  """class A {
      def print {
        1 match { case /*(*/ i /*)*/ => i }
      }
    }
  """ partitionsInto
  """→0(0)❨|→0(0)❨|class |A| |→0(0)❨|{
      |def| |print| |→6(6)❨|{
        |→8(2)❨|1| match { case /*(*/ |i| /*)*/ => |→8(0)❨|i|❩| }|❩|
      }|❩|
    }|❩|❩|
  |❩"""
  
  @Test
  def matchingWithType = 
  """class A {
      def print {
        1 match { case /*(*/ i: Int /*)*/ => i }
      }
    }
  """ partitionsInto
  """→0(0)❨|→0(0)❨|class |A| |→0(0)❨|{
      |def| |print| |→6(6)❨|{
        |→8(2)❨|1| match { case /*(*/ |i|: |Int| /*)*/ => |→8(0)❨|i|❩| }|❩|
      }|❩|
    }|❩|❩|
  |❩"""
  
  @Test
  def matchingWithBinding = 
  """class A {
      def print = {
        1 match { case /*(*/ a @ i /*)*/ => a }
      }
    }
  """ partitionsInto
  """→0(0)❨|→0(0)❨|class |A| |→0(0)❨|{
      |def| |print| = |→6(6)❨|{
        |→8(2)❨|1| match { case /*(*/ |a| @ |i| /*)*/ => |a| }|❩|
      }|❩|
    }|❩|❩|
  |❩"""
  
  @Test
  def multipleAssignment =
      """
    class A {
      def v() = (1,2,3,4)
      def assign {
        val (_1, _2, _3, _4) = v()
        val (a, b) = (1, 2)
      }
    }
  """ partitionsInto
  """→0(0)❨|
    |→4(4)❨|class |A| |→4(0)❨|{
      |def| |v|() = |→6(2)❨|(|1|,|2|,|3|,|4|)|❩|
      |def| |assign| |→6(2)❨|{
        |val| (|_1|, |_2|, |_3|, |_4|) = |v|()
        |val| (|a|, |b|) = |→8(2)❨|(|1|, |2|)|❩|
      }|❩|
    }|❩|❩|
  |❩"""

  @Test
  def newStatement = """
    object A {
      val a = new String("A")
    }
    """ partitionsInto """→0(0)❨|
    |→4(4)❨|object |A| |→4(0)❨|{
      |val| |a| = |new| |String|→6(2)❨|(|"A"|)|❩|
    }|❩|❩|
    |❩"""

  @Test
  def importQual = """
    object A {
      val p = scala.math.Pi
    }
    """ partitionsInto """→0(0)❨|
    |→4(4)❨|object |A| |→4(0)❨|{
      |val| |p| = |scala|.|math|.|Pi|
    }|❩|❩|
    |❩"""

  @Test
  def multiplication = """
    object A {
      val r = 3
      val p = r/**/* r
    }
    """ partitionsInto """→0(0)❨|
    |→4(4)❨|object |A| |→4(0)❨|{
      |val| |r| = |3|
      |val| |p| = |r|/**/|*| |r|
    }|❩|❩|
    |❩"""
  

}