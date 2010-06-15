/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests

import scala.tools.refactoring.implementations.ExtractLocal
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common._
import scala.tools.refactoring.analysis.IndexImplementations
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class ExtractLocalTest extends TestHelper with TestRefactoring {
  outer =>
  
  def extract(valName: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractLocal with SilentTracing with IndexImplementations {
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
    add(
    """
      package extractLocal
      object Demo {
        def update(platform: String) {

          println("Update..")

          if(platform/*(*/.toUpperCase.indexOf("MAC") > /*)*/-1) {
            println("We're on a Mac!")
          }
        }
      }
    """,
    """
      package extractLocal
      object Demo {
        def update(platform: String) {
          val isMacOs = platform/*(*/.toUpperCase.indexOf("MAC") > /*)*/-1

          println("Update..")

          if(isMacOs) {
            println("We're on a Mac!")
          }
        }
      }
    """)
  } applyRefactoring(extract("isMacOs"))
  
  @Test
  def extractLocal = new FileSet {
    add(
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
          
          val v = /*(*/3.14 * r * r/*)*/ * h

          println("volume is: "+ v)
        }
      }
    """,
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
          val gr = 3.14 * r * r/*)*/ 
          
          val v = /*(*/gr* h

          println("volume is: "+ v)
        }
      }
    """)
  } applyRefactoring(extract("gr"))
  
  @Test
  def extractNestedScopes = new FileSet {
    add(
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
    """,
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
    """)
  } applyRefactoring(extract("sum"))
}
