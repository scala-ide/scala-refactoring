package scala.tools.refactoring
package tests.implementations

import implementations.GenerateHashcodeAndEquals
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class GenerateHashcodeAndEqualsTest extends TestHelper with TestRefactoring {
  
  outer =>
    
  def generateHashcodeAndEquals(params: (Option[String => Boolean], Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new GenerateHashcodeAndEquals with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)
    }
    val changes = performRefactoring(params)
  }.changes

  @Test
  def singleValParam = new FileSet {
    """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String)
    """ becomes
    """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String) {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.singleValParam.Foo]
        }
        
        def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.singleValParam.Foo => that.canEqual(Foo.this).&&(param.==(that.param))
            case _ => false
          }
        }
        
        def hashCode() = {
          41.*(1).+(param.hashCode)
        }
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test
  def twoValParams = new FileSet {
    """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
    """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.twoValParams.Foo]
        }
        
        def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.twoValParams.Foo => that.canEqual(Foo.this).&&(p1.==(that.p1)).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        def hashCode() = {
          41.*(41.*(1).+(p1.hashCode)).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test
  def excludeNonPublicParams = new FileSet {
    """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int])
    """ becomes
    """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int]) {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.excludeNonPublicParams.Foo]
        }
        
        def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.excludeNonPublicParams.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        def hashCode() = {
          41.*(1).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test
  def excludeVarsByDefault = new FileSet {
    """
      package generateHashcodeAndEquals.excludeVarsByDefault

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int])
    """ becomes
    """
      package generateHashcodeAndEquals.excludeVarsByDefault

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.excludeVarsByDefault.Foo]
        }
        
        def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.excludeVarsByDefault.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        def hashCode() = {
          41.*(1).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test(expected=classOf[PreparationException])
  def failWithExistingEquals = new FileSet {
    """
      package generateHashcodeAndEquals.failWithExistingEquals

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def equals(other: Any) = false 
      }
    """ becomes
    """
      package generateHashcodeAndEquals.failWithExistingEquals

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def equals(other: Any) = false 
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test(expected=classOf[PreparationException])
  def failWithExistingHashCode = new FileSet {
    """
      package generateHashcodeAndEquals.failWithExistingHashCode

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def hashCode() = 57 
      }
    """ becomes
    """
      package generateHashcodeAndEquals.failWithExistingHashCode

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) {
        override def hashCode() = 57 
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, false)))
  
  @Test
  def selectByName = new FileSet {
    """
    package generateHashcodeAndEquals.selectByName
    
    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean])
    """ becomes
    """
    package generateHashcodeAndEquals.selectByName
    
    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean]) {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.selectByName.Foo]
      }
      
      def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.selectByName.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2)).&&(p3.==(that.p3))
          case _ => false
        }
      }
      
      def hashCode() = {
        41.*(41.*(1).+(p2.hashCode)).+(p3.hashCode)
      }
    }
    """
  } applyRefactoring(generateHashcodeAndEquals((Some{
    (name: String) => name match {
      case "p2" => true
      case "p3" => true
      case _ => false
    }
  }), false))
  
  @Test
  def callSuper = new FileSet {
    """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
    """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.callSuper.Foo]
        }
        
        def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.callSuper.Foo => Foo.super.equals(that).&&(that.canEqual(Foo.this)).&&(p1.==(that.p1)).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        def hashCode() = {
          41.*(41.*(Foo.super.hashCode()).+(p1.hashCode)).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring(generateHashcodeAndEquals((None, true)))
  
  @Test
  def foo = {
    val tree = treeFrom {
      """
        class Foo(var p1: String, val p2: Int) {
          def callSuper = super.equals("dummy")
        }
      """
    }
    assert(!(tree.toString contains "<error>"))
  }
  
}