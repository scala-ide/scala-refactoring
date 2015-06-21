package scala.tools.refactoring.tests
package implementations

import util.TestHelper
import util.TestRefactoring
import scala.tools.refactoring.implementations.ExtractTrait
import org.junit.Assert
import org.junit.Ignore
import language.reflectiveCalls
import scala.tools.refactoring.util.CompilerInstance
import org.junit.After

class ExtractTraitTest extends TestRefactoring {

  // We are experiencing instable test runs, maybe it helps when we
  // use a fresh compiler for each test case:

  override val global = (new CompilerInstance).compiler

  @After
  def shutdownCompiler() {
    global.askShutdown
  }

  def extractTrait(params: (String, String => Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractTrait with TestProjectIndex
    def filter(member: refactoring.global.ValOrDefDef) = params._2(member.symbol.nameString)
    val changes = performRefactoring(new refactoring.RefactoringParameters(params._1, filter))
  }.changes

  def extractTraitByParamListLength(params: (String, Int => Boolean))(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractTrait with TestProjectIndex
    def filter(member: refactoring.global.ValOrDefDef) = member match {
      case valdef: refactoring.global.ValDef => false
      case defdef: refactoring.global.DefDef => defdef.vparamss.headOption.map(vparams => params._2(vparams.size)).getOrElse(false)
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters(params._1, filter))
  }.changes

  @Test
  def extractNothing() = new FileSet {
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
  def extractNothingNestedPackages() = new FileSet {
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
  def extractSingleDefDef() = new FileSet {
    """
    package extractTrait.extractSingleDefDef

    class /*(*/A/*)*/ {
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
  def extractSingleValDef() = new FileSet {
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
  def extractSingleVarDef() = new FileSet {
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
  def dontExtractClassParameters() = new FileSet {
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
  def failWhenTraitAccessesPrivateClassMembers() = new FileSet {
    """
    package extractTrait.failWhenTraitAccessesPrivateClassMembers

    class /*(*/Foo/*)*/ {
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
  def failWhenClassAccessesPrivateTraitMembers() = new FileSet {
    """
    package extractTrait.failWhenClassAccessesPrivateTraitMembers

    class /*(*/Foo/*)*/ {
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
  def addSelfTypeForClassMemberAccess() = new FileSet {
    """
    package extractTrait.addSelfTypeForClassMemberAccess

    class /*(*/Foo/*)*/(val shared: Int) {
      def square() = shared*shared
    }
    """ becomes
    """
    package extractTrait.addSelfTypeForClassMemberAccess

    class /*(*/Foo/*)*/(val shared: Int) extends Squarify {
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
  def addSelfTypeForClassMethodAccess() = new FileSet {
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
  def withTypeParameters() = new FileSet {
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
  def withTypeParametersAndSelfType() = new FileSet {
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
  def withImportsMovedToTrait() = new FileSet {
    """
    package extractTrait.withImportsMovedToTrait
    import scala.math.abs

    class /*(*/LpNorms/*)*/(val vector: List[Int]) {
      def l1 = vector.map(prepare).sum
      def lInf = vector.map(prepare).max
      def prepare(value: Int) = abs(value)
    }
    """ becomes
    """
    package extractTrait.withImportsMovedToTrait

    class /*(*/LpNorms/*)*/(val vector: List[Int]) extends Preparator {
      def l1 = vector.map(prepare).sum
      def lInf = vector.map(prepare).max
    }
    """
    NewFile becomes
    """
    package extractTrait.withImportsMovedToTrait

    import scala.math.abs

    trait Preparator {
      def prepare(value: Int) = abs(value)
    }
    """
  } applyRefactoring(extractTrait(("Preparator"), (name) => name == "prepare"))

  @Test
  def withImportsOnlyInClass() = new FileSet {
    """
    package extractTrait.withImportsOnlyInClass

    import scala.math.abs

    class /*(*/Original/*)*/ {
      def l1(vector: List[Int]) = vector.map(prepare).sum
      def lInf(vector: List[Int]) = vector.map(prepare).max
      def prepare(value: Int) = abs(value)
    }
    """ becomes
    """
    package extractTrait.withImportsOnlyInClass

    import scala.math.abs

    class /*(*/Original/*)*/ extends LpNorms {
      def prepare(value: Int) = abs(value)
    }
    """
    NewFile becomes
    """
    package extractTrait.withImportsOnlyInClass

    trait LpNorms {
      this: Original =>
      def l1(vector: List[Int]) = vector.map(prepare).sum
      def lInf(vector: List[Int]) = vector.map(prepare).max
    }
    """
  } applyRefactoring(extractTrait(("LpNorms"), (name) => name.startsWith("l")))

  @Test
  def fromTrait() = new FileSet {
    """
    package extractTrait.fromTrait

    trait /*(*/Original/*)*/ {
      def square(a: Int) = a*a

      def cube(a: Int) = a*a*a
    }
    """ becomes
    """
    package extractTrait.fromTrait

    trait /*(*/Original/*)*/ extends Squarify {

      def cube(a: Int) = a*a*a
    }
    """
    NewFile becomes
    """
    package extractTrait.fromTrait

    trait Squarify {
      def square(a: Int) = a*a
    }
    """
  } applyRefactoring(extractTrait(("Squarify"), (name) => name == "square"))

  @Test
  def overloadedMethods() = new FileSet {
    """
    package extractTrait.overloadedMethods

    class /*(*/Overloaded/*)*/ {
      def overloaded(a: Int) = a
      def overloaded(a: Int, b: Int) = a+b
    }
    """ becomes
    """
    package extractTrait.overloadedMethods

    class /*(*/Overloaded/*)*/ extends OverloadedTrait {
      def overloaded(a: Int) = a
    }
    """
    NewFile becomes
    """
    package extractTrait.overloadedMethods

    trait OverloadedTrait {
      def overloaded(a: Int, b: Int) = a+b
    }
    """
  } applyRefactoring(extractTraitByParamListLength("OverloadedTrait", (nrParams) => nrParams == 2))

  @Test
  def inDefaultPackage() = new FileSet {
    """
    class /*(*/OriginalClassInDefaultPackage/*)*/ {
      def foo() {}
    }
    """ becomes
    """
    class /*(*/OriginalClassInDefaultPackage/*)*/ extends Foo {
    }
    """
    NewFile becomes
    """
    trait Foo {
      def foo() {}
    }
    """
  } applyRefactoring(extractTrait("Foo", (s) => s == "foo"))

  @Test
  def withExistingSuperclass() = new FileSet {
    """
    package withExistingSuperclass
    trait A {
     def foo(s: String)
    }
    class /*(*/Foo/*)*/ extends A {
      override def foo(s: String) {}
    }
    """ becomes
    """
    package withExistingSuperclass
    trait A {
     def foo(s: String)
    }
    class /*(*/Foo/*)*/ extends A with Extracted {
    }
    """
    NewFile becomes
    """
    package withExistingSuperclass

    trait Extracted {
      def foo(s: String) {}
    }
    """
  } applyRefactoring(extractTrait("Extracted", _ == "foo"))

  @Test
  def removeOverride() = new FileSet {
    """
    package extractTrait.removeOverride

    trait T {
      def foo(a: Int, b: Int) = a + b
    }

    class /*(*/C/*)*/ extends T {
      override def foo(a: Int, b: Int) = a * b
    }
    """ becomes
    """
    package extractTrait.removeOverride

    trait T {
      def foo(a: Int, b: Int) = a + b
    }

    class /*(*/C/*)*/ extends T with Extracted {
    }
    """
    NewFile becomes
    """
    package extractTrait.removeOverride

    trait Extracted {
      def foo(a: Int, b: Int) = a * b
    }
    """
  } applyRefactoring(extractTrait("Extracted", (s) => true))

}
