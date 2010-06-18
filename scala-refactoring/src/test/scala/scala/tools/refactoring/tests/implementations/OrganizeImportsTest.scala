/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package tests.implementations

import implementations.OrganizeImports
import tests.util.TestRefactoring
import tests.util.TestHelper

class OrganizeImportsTest extends TestHelper with TestRefactoring {
  outer =>
  
  def organize(pro: FileSet) = new TestRefactoringImpl(pro) {
    val refactoring = new OrganizeImports with SilentTracing {
      val global = outer.global
    }
    val changes = performRefactoring(new refactoring.RefactoringParameters)
  }.changes

  @Test
  def sort = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.Object
  
      object Main
    """ becomes
    """
      import java.lang.Object
      import scala.collection.mutable.ListBuffer
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def collapse = new FileSet {
    """
      import java.lang.String
      import java.lang.Object
  
      object Main
    """ becomes
    """
      import java.lang.{Object, String}
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def sortAndCollapse = new FileSet {
    """
      import scala.collection.mutable.ListBuffer
      import java.lang.String
      import java.lang.Object
  
      object Main
    """ becomes
    """
      import java.lang.{Object, String}
      import scala.collection.mutable.ListBuffer
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def collapseWithRename = new FileSet {
    """
      import java.lang.{String => S}
      import java.lang.{Object => O}
  
      object Main
    """ becomes
    """
      import java.lang.{Object => O, String => S}
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def importAll = new FileSet {
    """
      import java.lang._
      import java.lang.String
  
      object Main
    """ becomes
    """
      import java.lang._
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def importOnTrait = new FileSet {
    """
      package importOnTrait
      import java.lang._
      import java.lang.String
  
      trait A
  
      trait Main extends A {
      }
    """ becomes
    """
      package importOnTrait
      import java.lang._
  
      trait A
  
      trait Main extends A {
      }
    """
  } applyRefactoring organize
    
  @Test
  def importWithSpace = new FileSet {
    """
  
      import scala.collection.mutable.ListBuffer
      import java.lang.String
  
      object Main
    """ becomes
    """
  
      import java.lang.String
      import scala.collection.mutable.ListBuffer
  
      object Main
    """
  } applyRefactoring organize
    
  @Test
  def importAllWithRename = new FileSet {
    """
      import java.lang._
      import java.lang.{String => S}
  
      object Main
    """ becomes
    """
      import java.lang.{String => S, _}
  
      object Main
    """
  } applyRefactoring organize
}
