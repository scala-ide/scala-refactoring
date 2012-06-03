package scala.tools.refactoring
package tests.implementations

import implementations.GenerateHashcodeAndEquals
import tests.util.TestHelper
import tests.util.TestRefactoring
import org.junit.Ignore

class GenerateHashcodeAndEqualsTest extends TestHelper with TestRefactoring {

  outer =>

  def generateHashcodeAndEquals(params: (Boolean, Option[String => Boolean]))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new GenerateHashcodeAndEquals with SilentTracing {
      val global = outer.global
    }
    import refactoring.global.ValDef
    val paramsFilter = params._2.map(strFilter => (param: ValDef) => strFilter(param.name.toString))
    val changes = performRefactoring(refactoring.RefactoringParameters(params._1, paramsFilter))
  }.changes

  @Test
  def singleValParam = new FileSet {
    """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String)
    """ becomes
      """
      package generateHashcodeAndEquals.singleValParam

      class /*(*/Foo/*)*/(val param: String) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.singleValParam.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.singleValParam.Foo => that.canEqual(Foo.this).&&(param.==(that.param))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(1).+(param.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test
  def twoValParams = new FileSet {
    """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.twoValParams

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.twoValParams.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.twoValParams.Foo => that.canEqual(Foo.this).&&(p1.==(that.p1)).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(prime.*(1).+(p1.hashCode)).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test
  def excludeNonPublicParams = new FileSet {
    """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.excludeNonPublicParams

      class /*(*/Foo/*)*/(p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.excludeNonPublicParams.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.excludeNonPublicParams.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(1).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test
  def excludeVarsByDefault = new FileSet {
    """
      package generateHashcodeAndEquals.excludeVarsByDefault

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.excludeVarsByDefault

      class /*(*/Foo/*)*/(var p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.excludeVarsByDefault.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.excludeVarsByDefault.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(1).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test(expected = classOf[PreparationException])
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
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test(expected = classOf[PreparationException])
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
  } applyRefactoring (generateHashcodeAndEquals((false, None)))

  @Test
  def selectByName = new FileSet {
    """
    package generateHashcodeAndEquals.selectByName
    
    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean])
    """ becomes
      """
    package generateHashcodeAndEquals.selectByName
    
    class /*(*/Foo/*)*/(val p1: String, val p2: List[Int], var p3: Boolean, val p4: List[Boolean]) extends Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.selectByName.Foo]
      }
      
      override def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.selectByName.Foo => that.canEqual(Foo.this).&&(p2.==(that.p2)).&&(p3.==(that.p3))
          case _ => false
        }
      }
      
      override def hashCode() = {
        val prime = 41
        prime.*(prime.*(1).+(p2.hashCode)).+(p3.hashCode)
      }
    }
    """
  } applyRefactoring (generateHashcodeAndEquals(false, (Some {
    (name: String) =>
      name match {
        case "p2" => true
        case "p3" => true
        case _ => false
      }
  })))

  @Test
  def callSuper = new FileSet {
    """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int])
    """ becomes
      """
      package generateHashcodeAndEquals.callSuper

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.callSuper.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.callSuper.Foo => Foo.super.equals(that).&&(that.canEqual(Foo.this)).&&(p1.==(that.p1)).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(prime.*(Foo.super.hashCode()).+(p1.hashCode)).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((true, None)))

  @Test
  def emptyClassBody = new FileSet {
    """
      package generateHashcodeAndEquals.emptyClassBody

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) {
    
      }
    """ becomes
      """
      package generateHashcodeAndEquals.emptyClassBody

      class /*(*/Foo/*)*/(val p1: String, val p2: List[Int]) extends Equals {
        def canEqual(other: Any) = {
          other.isInstanceOf[generateHashcodeAndEquals.emptyClassBody.Foo]
        }
        
        override def equals(other: Any) = {
          other match {
            case that: generateHashcodeAndEquals.emptyClassBody.Foo => Foo.super.equals(that).&&(that.canEqual(Foo.this)).&&(p1.==(that.p1)).&&(p2.==(that.p2))
            case _ => false
          }
        }
        
        override def hashCode() = {
          val prime = 41
          prime.*(prime.*(Foo.super.hashCode()).+(p1.hashCode)).+(p2.hashCode)
        }
      }
    """
  } applyRefactoring (generateHashcodeAndEquals((true, None)))
  
  @Test
  def noParams = new FileSet {
    """
    package generateHashcodeAndEquals.noParams
    class /*(*/Foo/*)*/
    """  becomes
    """
    package generateHashcodeAndEquals.noParams
    class /*(*/Foo extends Equals {
      def canEqual(other: Any) = {
        other.isInstanceOf[generateHashcodeAndEquals.noParams.Foo]
      }
      
      override def equals(other: Any) = {
        other match {
          case that: generateHashcodeAndEquals.noParams.Foo => that.canEqual(Foo.this)
          case _ => false
        }
      }
      
      override def hashCode() = {
        val prime = 41
        prime
      }
    }/*)*/
    """
  } applyRefactoring(generateHashcodeAndEquals((false, Some((p) => false))))

}