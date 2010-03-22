package scala.tools.refactoring.tests

import scala.tools.refactoring.implementations.ExtractLocal
import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.common.Tracing
import scala.tools.refactoring.common.SilentTracing
import scala.tools.refactoring.analysis.FullIndexes
import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._

class ExtractLocalTest extends TestHelper with TestRefactoring {
  
  def extract(valName: String)(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new ExtractLocal(global) with Tracing with FullIndexes {
      pro.trees map (_.pos.source.file) map (file => global.unitOfFile(file).body) foreach ( index processTree _ )
      
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

          val v = /*(*/scala.math.Pi * r * r/*)*/ * h

          println("volume is: "+ v)
        }
      }
    """,
    """
      package extractLocal
      object Demo {
        def printVolume(r: Double, h: Double) {
          val gr = /*(*/scala.math.Pi * r * r/*)*/ 

          val v = /*(*/gr/*)*/ * h

          println("volume is: "+ v)
        }
      }
    """)
  } applyRefactoring(extract("gr"))
}
