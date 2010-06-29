/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import tests.util.TestRefactoring
import implementations.ExtractMethod
import tests.util.TestHelper

class ExtractMethodTest extends TestHelper with TestRefactoring {
  outer =>
  
  def extract(name: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractMethod with SilentTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes) 
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters {
      val methodName = name
    })
  }.changes

  @Test
  def extractBlock = new FileSet {
    """
    package extractBlock
    class A {
      def extractFrom: Int = {
/*(*/   val a = {
          val b = 1
          b * 5
        }   /*)*/
        a * a
      }
    }
    """ becomes
    """
    package extractBlock
    class A {
      def extractFrom: Int = {
        val a = prntln
        a * a
      }
      def prntln: Int = {
/*(*/   val a = {
          val b = 1
          b * 5
        }   /*)*/
        a
      }
    }
    """
  } applyRefactoring extract("prntln")
    
  @Test
  def simpleExtract = new FileSet {
    """
    package simpleExtract
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }
    """ becomes
    """
    package simpleExtract
    class A {
      def extractFrom {
        myOwnPrint
        ()
      }
      def myOwnPrint: Unit = {
/*(*/   println("hello")/*)*/
      }
    }
    """
  } applyRefactoring extract("myOwnPrint")
    
  @Test
  def ignoreOtherClass = new FileSet {
    """
    package ignoreOtherClass
    class A {
      def extractFrom {
/*(*/   println("hello")/*)*/
        ()
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
    """ becomes
    """
    package ignoreOtherClass
    class A {
      def extractFrom {
        prntln
        ()
      }
      def prntln: Unit = {
/*(*/   println("hello")/*)*/
      }
    }

    class A2(s: String)
    class B(s: String) extends A2(s)
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractOneParameter = new FileSet {
    """
    package simpleExtractOneParameter
    class A {
      def extractFrom {
        val a = 1
/*(*/   println(a)  /*)*/
        ()
      }
    }
    """ becomes
    """
    package simpleExtractOneParameter
    class A {
      def extractFrom {
        val a = 1
        prntln(a)
        ()
      }
      def prntln(a: Int): Unit = {
/*(*/   println(a)  /*)*/
      }
    }
    """
  } applyRefactoring extract("prntln")

  @Test
  def simpleExtractSeveralParameters = new FileSet {
    """
    package simpleExtractSeveralParameters
    class A {
      def extractFrom(d: Int) {
        val a = 1
        val b = 1
        val c = 1
/*(*/   println(a + b + c + d)  /*)*/
        ()
      }
    }
    """ becomes
    """
    package simpleExtractSeveralParameters
    class A {
      def extractFrom(d: Int) {
        val a = 1
        val b = 1
        val c = 1
        prntln(d, a, b, c)
        ()
      }
      def prntln(d: Int, a: Int, b: Int, c: Int): Unit = {
/*(*/   println(a + b + c + d)  /*)*/
      }
    }
    """
  } applyRefactoring extract("prntln")
    
  @Test
  def simpleExtractReturn = new FileSet {
    """
    package simpleExtractReturn
    class A {
      def extractFrom() {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """ becomes
    """
    package simpleExtractReturn
    class A {
      def extractFrom() {
        val a = prntln
        a
      }
      def prntln: Int = {
/*(*/   val a = 1  /*)*/
        a
      }
    }
    """
  } applyRefactoring extract("prntln")
    
  @Test
  def simpleExtractMultipleReturns = new FileSet {
    """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
/*(*/   val a = 1
        val b = 1  /*)*/
        a + b
      }
    }
    """ becomes
    """
    package simpleExtractMultipleReturns
    class A {
      def extractFrom() {
        val (a, b) = prntln
        a + b
      }
      def prntln: (Int, Int) = {
/*(*/   val a = 1
        val b = 1  /*)*/
        (a, b)
      }
    }
    """
  } applyRefactoring extract("prntln")
    
  @Test
  def simpleExtractParametersAndReturns = new FileSet {
    """
    package simpleExtractParametersAndReturns
    class A {
      def extractFrom() {
        val a = 1
        val b = 1
        val c = 1
/*(*/   val d = a + c
        val e = d + a  /*)*/
        a+b+c+d+e
      }
    }
    """ becomes
    """
    package simpleExtractParametersAndReturns
    class A {
      def extractFrom() {
        val a = 1
        val b = 1
        val c = 1
        val (d, e) = prntln(a, c)
        a+b+c+d+e
      }
      def prntln(a: Int, c: Int): (Int, Int) = {
/*(*/   val d = a + c
        val e = d + a  /*)*/
        (d, e)
      }
    }
    """
  } applyRefactoring extract("prntln")
    
  @Test
  def extractBlockExpression = new FileSet {
    """
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
/*(*/   a + 1    /*)*/
      }
    }
    """ becomes
    """
    package extractBlockExpression
    class A {
      def extractFrom(): Int = {
        val a = 1
        inc(a)    /*)*/
      }
      def inc(a: Int): Int = {
/*(*/   a + 1
      }
    }
    """
  } applyRefactoring extract("inc")
    
  @Test
  def replaceWholeMethod = new FileSet {
    """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
/*(*/   val a = 1
        a + 1    /*)*/
      }
    }
    """ becomes
    """
    package replaceWholeMethod
    class A {
      def extractFrom(): Int = {
        inc    /*)*/
      }
      def inc: Int = {
/*(*/   val a = 1
        a + 1
      }
    }
    """
  } applyRefactoring extract("inc")
    
  @Test
  def extractIfCond = new FileSet {
    """
    package extractIfCond
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/( /*(*/ true == true /*)*/ )
          true
        else
          false 
      }
    }
    """ becomes
    """
    package extractIfCond
    class A {
      def extractFrom(): Boolean = {
        if/*aa*/(test)
          true
        else
          false 
      }
      def test: Boolean = {
         /*(*/ true == true /*)*/ 
      }
    }
    """
  } applyRefactoring extract("test")
        
  @Test
  def extractIfThen = new FileSet {
    """
    package extractIfThen
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
 /*(*/    true /*)*/
        else
          false 
      }
    }
    """ becomes
    """
    package extractIfThen
    class A {
      def extractFrom(): Boolean = {
        if(true == true)test /*)*/
        else
          false 
      }
      def test: Boolean = {
 /*(*/    true
      }
    }
    """
  } applyRefactoring extract("test")
    
  @Test
  def extractIfElse = new FileSet {
    """
    package extractIfElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
 /*(*/    false /*)*/
        }
      }
    }
    """ becomes
    """
    package extractIfElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true)
          true
        else {
 /*(*/    test /*)*/
        }
      }
      def test: Boolean = {
        false
      }
    }
    """
  } applyRefactoring extract("test")
    
  @Test
  def extractIfSingleLineElse = new FileSet {
    """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ false /*)*/
      }
    }
    """ becomes
    """
    package extractIfSingleLineElse
    class A {
      def extractFrom(): Boolean = {
        if(true == true) true else /*(*/ test /*)*/
      }
      def test: Boolean = {
        false
      }
    }
    """
  } applyRefactoring extract("test")
    
  @Test
  def extractIfElseTry = new FileSet {
    """
    package extractIfElseTry
    class A {
      def extractFrom(): Boolean = {
        if(true == true) /*(*/  true  /*)*/
        else {
          try {
            println("hello world")
            if(true) true
            else false
          } catch {
            case _ => false
          }
        }
      }
    }
    """ becomes
    """
    package extractIfElseTry
    class A {
      def extractFrom(): Boolean = {
        if(true == true)test  /*)*/
        else {
          try {
            println("hello world")
            if(true) true
            else false
          } catch {
            case _ => false
          }
        }
      }
      def test: Boolean = {
         /*(*/  true
      }
    }
    """
  } applyRefactoring extract("test")

  @Test
  def extractCheckForFalse = new FileSet {
    """
          package extractCheckForFalse
      trait Check {
        def whatIsIt(check: Boolean) {
          if (/*(*/check == false/*)*/ /*hi*/)
            println("It's false")
          else
            println("It's true")
        }
      }
    """ becomes
    """
          package extractCheckForFalse
      trait Check {
        def whatIsIt(check: Boolean) {
          if (isFalse(check))
            println("It's false")
          else
            println("It's true")
        }
        def isFalse(check: Boolean): Boolean = {
          /*(*/check == false/*)*/ /*hi*/
        }
      }
    """
  } applyRefactoring extract("isFalse")
    
  @Test
  def extractWithMethod = new FileSet {
    """
    package extractWithMethod
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """ becomes
    """
    package extractWithMethod
    class A {
      def extractFrom(): Boolean = {
        val invert: Boolean => Boolean = ! _
        val a = false
        val b = certainlyTrue(invert, a)
        b
      }
      def certainlyTrue(invert: (Boolean) => Boolean, a: Boolean): Boolean = {
/*(*/   val b = invert(a)    /*)*/
        b
      }
    }
    """
  } applyRefactoring extract("certainlyTrue")

  @Test
  def extractAllAtOnce = new FileSet {
    """
    package extractAllAtOnce
    object C {
      def calculate {
        val sumList: Seq[Int] => Int = _ reduceLeft (_+_)
        val prodList: Seq[Int] => Int = _ reduceLeft (_*_)
        val values = 1 to 10 toList
    /*(*/    val     sum = sumList(values)   // the sum
        val product = prodList(values) /*)*/ // the product
    
        println("The sum from 1 to 10 is "+ sum +"; the product is "+ product)
      }
    }
  """ becomes
  """
    package extractAllAtOnce
    object C {
      def calculate {
        val sumList: Seq[Int] => Int = _ reduceLeft (_+_)
        val prodList: Seq[Int] => Int = _ reduceLeft (_*_)
        val values = 1 to 10 toList
        val (sum, product) = magic(sumList, prodList, values)
    
        println("The sum from 1 to 10 is "+ sum +"; the product is "+ product)
      }
      def magic(sumList: (Seq[Int]) => Int, prodList: (Seq[Int]) => Int, values: List[Int]): (Int, Int) = {
    /*(*/    val     sum = sumList(values)   // the sum
        val product = prodList(values) /*)*/ // the product
        (sum, product)
      }
    }
  """
  } applyRefactoring extract("magic")
  
  @Test
  def extractLarger = new FileSet {
    """
    package extractLarger
    object C {
      def whatIsIt(check: Boolean) {
        if (/*(*/check == false/*)*/ /*hi*/)
          println("It's false")
        else
          println("It's true")
      }
    
      def unrelated1 {
        println("unrelated1")
      }
    
      def unrelated2 {
        println("unrelated2")
      }
    
      def unrelated3 {
        println("unrelated3")
      }
    }
    
    object c2 {
      def blabla {
        println("blabla")
      }
    }
  """ becomes
  """
    package extractLarger
    object C {
      def whatIsIt(check: Boolean) {
        if (isFalse(check))
          println("It's false")
        else
          println("It's true")
      }
    
      def unrelated1 {
        println("unrelated1")
      }
    
      def unrelated2 {
        println("unrelated2")
      }
    
      def unrelated3 {
        println("unrelated3")
      }
      def isFalse(check: Boolean): Boolean = {
        /*(*/check == false/*)*/ /*hi*/
      }
    }
    
    object c2 {
      def blabla {
        println("blabla")
      }
    }
  """
  } applyRefactoring extract("isFalse")
}
