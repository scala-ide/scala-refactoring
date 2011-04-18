/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.OrganizeImports
import tests.util.{TestHelper, TestRefactoring}
import implementations.UnusedImportsFinder

class UnusedImportsFinderTest extends TestHelper {
  outer =>
    
  def findUnusedImports(expected: String, src: String) {
    
   val unuseds = {
      new UnusedImportsFinder {
  
        val global = outer.global
        
        val unit = global.unitOfFile(addToCompiler(randomFileName(), src))
        
        def compilationUnitOfFile(f: AbstractFile) = Some(unit)
        
        val unuseds = findUnusedImports(unit)
      }.unuseds
    } 
    
   org.junit.Assert.assertEquals(expected, unuseds.mkString(", "))
  }
    
  @Test
  def simpleUnusedType() = findUnusedImports(
    "(ListBuffer,2)", 
    """
      import scala.collection.mutable.ListBuffer
  
      object Main {val s: String = "" }
    """
  )
    
  @Test
  def typeIsUsedAsVal() = findUnusedImports(
    "", 
    """
      import scala.collection.mutable.ListBuffer
  
      object Main {val s = new ListBuffer[Int] }
    """
  )
    
  @Test
  def typeIsImportedFrom() = findUnusedImports(
    "", 
    """
      class Forest {
        class Tree
      }

      class UsesTrees {
        val forest = new Forest
        import forest._
        val x = new Tree
      }
    """
  )
  
  // more tests are in organize imports
}
