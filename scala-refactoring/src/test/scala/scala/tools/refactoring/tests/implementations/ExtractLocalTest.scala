/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package tests.implementations

import implementations.ExtractLocal
import tests.util.TestRefactoring
import tests.util.TestHelper
import org.junit.Assert._

class ExtractLocalTest extends TestHelper with TestRefactoring {
  outer =>
  
  def extract(valName: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractLocal with ConsoleTracing with GlobalIndexes {
      val global = outer.global
      val cuIndexes = pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) map CompilationUnitIndex.apply
      val index = GlobalIndex(cuIndexes)      
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters {
      val name = valName
    })
  }.changes
  
  @Test
  def extractIfCond = new FileSet {
    """
      package extractLocal
      object Demo {
        def update(platform: String) {
          println("Update..")
          if(/*(*/platform.toUpperCase.indexOf("MAC") > -1/*)*/) {
            println("We're on a Mac!")
          }
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def update(platform: String) {
          println("Update..")
          val isMacOs = /*(*/platform.toUpperCase.indexOf("MAC") > -1/*)*/
          if(isMacOs) {
            println("We're on a Mac!")
          }
        }
      }
    """
  } applyRefactoring(extract("isMacOs"))
  
  @Test
  def extractLocal = new FileSet {
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
          
          val v = /*(*/3.14 * r * r/*)*/ * h

          println("volume is: "+ v)
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
          val gr = 3.14 * r * r/*)*/ 
          
          val v = /*(*/gr* h

          println("volume is: "+ v)
        }
      }
    """
  } applyRefactoring(extract("gr"))
  
  @Test
  def extractNestedScopes = new FileSet {
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]) {

          println("Printing the sum..")
          
          if(l.isEmpty) {
            println("is empty :-(")
          } else
            println("sum is: "+  /*(*/l.reduceLeft(_ + _)/*)*/  )

          println(".. done")
        }
      }
    """ becomes
    """
      package extractLocal
      object Demo {
        def printSum(l: List[Int]) {

          println("Printing the sum..")
          
          if(l.isEmpty) {
            println("is empty :-(")
          } else {
            val sum = l.reduceLeft(_ + _)
            println("sum is: "+  /*(*/sum/*)*/  )
          }

          println(".. done")
        }
      }
    """
  } applyRefactoring(extract("sum"))
  
  @Test
  def extractValRhs = new FileSet {
    """
      object Demo {
        def update(platform: String) {
          val s = /*(*/platform/*)*/
        }
      }
    """ becomes
    """
      object Demo {
        def update(platform: String) {
          val plt = /*(*/platform
          val s = plt/*)*/
        }
      }
    """
  } applyRefactoring(extract("plt"))
  
  @Test
  def extractValRhs2 = new FileSet {
    """
      class Extr {
        def update(platform: String) {
          val s = ( /*(*/2*3/*)*/) + 1
        }
      }
    """ becomes
    """
      class Extr {
        def update(platform: String) {
          val six = 2*3/*)*/
          val s = six + 1
        }
      }
    """
  } applyRefactoring(extract("six"))
  
  @Test
  def extractFilter = new FileSet {
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList

     /*(*/list filter (_ > 3)/*)*/ filter (_ < 6)
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          val list = (1 to 10) toList

     /*(*/  val largerThree = list filter (_ > 3)/*)*/ 
          largerThree filter (_ < 6)
        }
      }
    """
  } applyRefactoring(extract("largerThree"))
  
  @Test
  def extractPartOfACondition = new FileSet {
    """
      class Extr2 {
        def m {
          val someValue = true
          if(someValue && /*(*/"aa".matches("\\w+")/*)*/) {
            println("yay")
          }
        }
      }
    """ becomes
    """
      class Extr2 {
        def m {
          val someValue = true
          val part2 = "aa".matches("\\w+")
          if(someValue && /*(*/part2/*)*/) {
            println("yay")
          }
        }
      }
    """
  } applyRefactoring(extract("part2"))
  
  @Test
  def extractFromCaseWithMultipleStatements = new FileSet {
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val a = 5
            val b = /*(*/a + 1/*)*/
            b
        }
      }
    """ becomes
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val a = 5
            val six = /*(*/a + 1
            val b = six/*)*/
            b
        }
      }
    """
  } applyRefactoring(extract("six"))
  
  @Test
  def extractFromCaseWithSingleStatement = new FileSet {
    """
      class Extr2 {
        Nil match {
          case Nil =>
            /*(*/5 + 1/*)*/ toString
        }
      }
    """ becomes
    """
      class Extr2 {
        Nil match {
          case Nil =>
            val six = 5 + 1/*)*/ 
            /*(*/six toString
        }
      }
    """
  } applyRefactoring(extract("six"))
  
  @Test
  def extractFromCaseWithTwoStatements = new FileSet {
    """
      class Extr2 {
        /*(*/5 + 1/*)*/ toString
      }
    """ becomes
    """
      class Extr2 {
        val six = 5 + 1/*)*/ 
        /*(*/six toString
      }
    """
  } applyRefactoring(extract("six"))
  
  @Test
  def extractFromTry = new FileSet {
    """
      class Extr2 {
        try {
          val a = List(1,2,3)
          /*(*/a filter (_> 2)/*)*/ mkString ", "
        }
      }
    """ becomes
    """
      class Extr2 {
        try {
          val a = List(1,2,3)
          val largerThanTwo = a filter (_> 2)/*)*/ 
          /*(*/largerThanTwo mkString ", "
        }
      }
    """
  } applyRefactoring(extract("largerThanTwo"))
  
  @Test
  def extractFromTrySingleStatement = new FileSet {
    """
      class Extr2 {
        try {
          /*(*/List(1,2,3) filter (_> 2)/*)*/ mkString ", "
        }
      }
    """ becomes
    """
      class Extr2 {
        try {
          val largerThanTwo = List(1,2,3) filter (_> 2)/*)*/ 
          /*(*/largerThanTwo mkString ", "
        }
      }
    """
  } applyRefactoring(extract("largerThanTwo"))
}
