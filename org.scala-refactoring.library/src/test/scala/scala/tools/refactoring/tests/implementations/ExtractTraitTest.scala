package scala.tools.refactoring.tests
package implementations

import util.TestHelper
import util.TestRefactoring
import scala.tools.refactoring.implementations.ExtractTrait
import org.junit.Assert
import org.junit.Ignore

class ExtractTraitTest extends TestHelper with TestRefactoring {

  outer =>
    
  def extractTrait(params: (String, String => Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractTrait with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters(params._1, params._2))
  }.changes
  
  @Test
  def extractNothing = new FileSet {
    """
    package extractTrait.extractNothing
    class /*(*/Foo/*)*/
    """ becomes
    """
    package extractTrait.extractNothing
    class /*(*/Foo extends Extracted/*)*/
    """
    NewFile becomes
    """
    package extractTrait.extractNothing
    trait Extracted/*)*/
    """
  } applyRefactoring(extractTrait(("Extracted", (name) => false)))
  
  @Test
  def extractNothingNestedPackages = new FileSet {
    """
    package a {
      package b.c {
        package d {
          package e.f.g {
            class /*(*/Foo/*)*/
          }
        }
      }
    }
    """ becomes
    """
    package a {
      package b.c {
        package d {
          package e.f.g {
            class /*(*/Foo extends Extracted/*)*/
          }
        }
      }
    }
    """
    NewFile becomes
    """
    package a.b.c.d.e.f.g
    trait Extracted
    """
  } applyRefactoring(extractTrait(("Extracted", (name) => false)))
  
  @Test
  def extractSingleDefDef = new FileSet {
    """
    package extractTrait.extractSingleDefDef
    
    class /*(*/A/*)*/{
      def square(a: Int) = a*a
    
      def cube(a: Int) = a*a*a
    }
    """ becomes
    """
    package extractTrait.extractSingleDefDef
    
    class /*(*/A/*)*/ extends Squarify {
    
      def cube(a: Int) = a*a*a
    }
    """
    NewFile becomes
    """
    package extractTrait.extractSingleDefDef
    trait Squarify {
      def square(a: Int) = a*a
    }
    """
  } applyRefactoring(extractTrait(("Squarify", (name) => name == "square")))
  
  @Test
  def extractSingleValDef = new FileSet {
    """
    package extractTrait.extractSingleValDef
    
    class /*(*/Foo/*)*/{
      val immutable = 57
      var mutable = "57"
    }
    """ becomes
    """
    package extractTrait.extractSingleValDef
    
    class /*(*/Foo/*)*/ extends Extracted {
      var mutable = "57"
    }
    """
    NewFile becomes
    """
    package extractTrait.extractSingleValDef
    trait Extracted {
      val immutable = 57
    }
    """
  } applyRefactoring(extractTrait(("Extracted", (name) => name == "immutable")))
  
  @Test
  def extractSingleVarDef = new FileSet {
    """
    package extractTrait.extractSingleVarDef
    
    class /*(*/Foo/*)*/{
      val immutable = 57
      var mutable = "57"
    }
    """ becomes
    """
    package extractTrait.extractSingleVarDef
    
    class /*(*/Foo/*)*/ extends Extracted {
      val immutable = 57
    }
    """
    NewFile becomes
    """
    package extractTrait.extractSingleVarDef
    trait Extracted {
      var mutable = "57"
    }
    """
  } applyRefactoring(extractTrait(("Extracted", (name) => name == "mutable")))  
  
  @Test
  def dontExtractClassParameters = new FileSet {
    """
    package extractTrait.dontExtractClassParameters
    
    class /*(*/Foo/*)*/(a: Int, val b: String, var c: Int) {
      def bar() = 57
    }
    """ becomes
    """
    package extractTrait.dontExtractClassParameters
    
    class /*(*/Foo/*)*/(a: Int, val b: String, var c: Int) extends EmptyTrait {
      def bar() = 57
    }
    """
    NewFile becomes
    """
    package extractTrait.dontExtractClassParameters
    trait EmptyTrait
    """
  } applyRefactoring(extractTrait(("EmptyTrait"), (name) => "a"::"b"::"c"::Nil contains name))
  
  @Test(expected=classOf[RefactoringException])
  def failWhenTraitAccessesPrivateClassMembers = new FileSet {
    """
    package extractTrait.failWhenTraitAccessesPrivateClassMembers
    
    class /*(*/Foo/*)*/{
      private val number = 17
      def square = number*number
    }
    """ becomes
    """
    package extractTrait.failWhenTraitAccessesPrivateClassMembers
    
    class /*(*/Foo/*)*/ extends Squarify {
      private val number = 17
    }
    """
    NewFile becomes
    """
    package extractTrait.failWhenTraitAccessesPrivateClassMembers
    trait Squarify {
      def square = number*number
    }
    """
  } applyRefactoring(extractTrait(("Squarify"), (name) => name == "square"))
  
  @Test(expected=classOf[RefactoringException])
  def failWhenClassAccessesPrivateTraitMembers = new FileSet {
    """
    package extractTrait.failWhenClassAccessesPrivateTraitMembers
    
    class /*(*/Foo/*)*/{
      private val number = 17
      def square = number*number
    }
    """ becomes
    """
    package extractTrait.failWhenClassAccessesPrivateTraitMembers
    
    class /*(*/Foo/*)*/ extends Squarify {
      def square = number*number
    }
    """
    NewFile becomes
    """
    package extractTrait.failWhenClassAccessesPrivateTraitMembers
    trait Squarify {
      private val number = 17
    }
    """
  } applyRefactoring(extractTrait(("Squarify"), (name) => name == "number"))
  
  @Test
  //TODO: fix parenthesis
  def addSelfTypeForClassMemberAccess = new FileSet {
    """
    package extractTrait.addSelfTypeForClassMemberAccess
    
    class /*(*/Foo/*)*/(val shared: Int) {
      def square() = shared*shared
    }
    """ becomes
    """
    package extractTrait.addSelfTypeForClassMemberAccess
    
    class /*(*/Foo/*)*/(val shared: Int) {) extends Squarify
    }
    """
    NewFile becomes
    """
    package extractTrait.addSelfTypeForClassMemberAccess
    trait Squarify {
      this: Foo =>
      def square() = shared*shared
    }
    """
  } applyRefactoring(extractTrait(("Squarify"), (name) => name == "square"))
  
  @Test
  def addSelfTypeForClassMethodAccess = new FileSet {
    """
    package extractTrait.addSelfTypeForClassMethodAccess
    
    class /*(*/Foo/*)*/{
      def square() = getNumber * getNumber
      def getNumber() = 57
    }
    """ becomes
    """
    package extractTrait.addSelfTypeForClassMethodAccess
    
    class /*(*/Foo/*)*/ extends Squarify {
      def getNumber() = 57
    }
    """
    NewFile becomes
    """
    package extractTrait.addSelfTypeForClassMethodAccess
    trait Squarify {
      this: Foo =>
      def square() = getNumber * getNumber
    }
    """
  } applyRefactoring(extractTrait(("Squarify"), (name) => name == "square"))
  
  @Test
  def withTypeParameters = new FileSet {
    """
    package extractTrait.withTypeParameters
    
    class /*(*/Foo[A]/*)*/(val list: List[A]) {
      def prepend(a: A) = new Foo(a::list)
      def initial: Foo[A] = new Foo(empty)
      def empty: List[A] = Nil
    }
    """ becomes
    """
    package extractTrait.withTypeParameters
    
    class /*(*/Foo[A]/*)*/(val list: List[A]) extends Empty[A] {
      def prepend(a: A) = new Foo(a::list)
      def initial: Foo[A] = new Foo(empty)
    }
    """
    NewFile becomes
    """
    package extractTrait.withTypeParameters
    trait Empty[A]/*)*/ {
      def empty: List[A] = Nil
    }
    """
  } applyRefactoring(extractTrait(("Empty"), (name) => name == "empty"))
  
  @Test
  def withTypeParametersAndSelfType = new FileSet {
    """
    package extractTrait.withTypeParametersAndSelfType
    
    class /*(*/Foo[A, B]/*)*/(val first: List[A], val second: List[B]) {
      def reverseFirst = first.reverse
      def reverseSecond = second.reverse
    }
    """ becomes
    """
    package extractTrait.withTypeParametersAndSelfType
    
    class /*(*/Foo[A, B]/*)*/(val first: List[A], val second: List[B]) extends FirstReverser[A, B] {
      def reverseSecond = second.reverse
    }
    """
    NewFile becomes
    """
    package extractTrait.withTypeParametersAndSelfType
    trait FirstReverser[A, B]/*)*/ {
      this: Foo[A, B] =>
      def reverseFirst = first.reverse
    }
    """
  } applyRefactoring(extractTrait(("FirstReverser"), (name) => name == "reverseFirst"))
  
  @Test
  def foo {
    val tree = treeFrom{
      """
      class Empty[A, B] {
      def empty: List[A] = Nil
      }
      """
    }
    Assert.assertFalse(tree.toString contains "<error>")
  }
  
}