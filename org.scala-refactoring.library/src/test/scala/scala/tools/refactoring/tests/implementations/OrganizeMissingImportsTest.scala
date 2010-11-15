/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package tests.implementations

import implementations.OrganizeImports
import tests.util.TestRefactoring
import tests.util.TestHelper

class OrganizeMissingImportsTest extends TestHelper with TestRefactoring {
  outer =>
  
  def organize(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  @Test
  def applyall = new FileSet {
    """
      object Main {val lb = ListBuffer(1)}
    """ becomes
    """"""
  } applyRefactoring organize
    
  @Test
  def parameter = new FileSet {
    """
      object Main { def method(l: Vector) = "" }
    """ becomes
    """"""
  } applyRefactoring organize
  
  @Test
  def returnValue = new FileSet {
    """
      object Main { def method(): ListBuffer = new collection.mutable.ListBuffer() }
    """ becomes
    """"""
  } applyRefactoring organize
  
  @Test
  def newInstance = new FileSet {
    """
      object Main { def method() = new ListBuffer() }
    """ becomes
    """"""
  } applyRefactoring organize
  
  @Test
  def newInstance2 = new FileSet {
    """
      object Main { def method() = new mutable.ListBuffer() }
    """ becomes
    """"""
  } applyRefactoring organize
    
  @Test
  def importFromMissingImport = new FileSet {
    """
      object Main { import ListBuffer._ }
    """ becomes
    """"""
  } applyRefactoring organize
    
  @Test
  def missingSuperclass = new FileSet {
    """
      class Subclass extends LinkedList
    """ becomes
    """"""
  } applyRefactoring organize

}
