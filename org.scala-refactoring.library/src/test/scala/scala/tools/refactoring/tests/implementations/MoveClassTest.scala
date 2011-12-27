package scala.tools.refactoring
package tests.implementations

import org.junit.Test

import implementations.MoveClass
import tests.util.{TestRefactoring, TestHelper}

class MoveClassTest extends TestHelper with TestRefactoring {
    
  private def moveTo(target: String)(pro: FileSet) = {
    val testRefactoring = new TestRefactoringImpl(pro) {
      val refactoring = new MoveClass with ConsoleTracing with TestProjectIndex
    }
    testRefactoring.performRefactoring(testRefactoring.refactoring.TargetPackage(target))
  }
  
  @Test
  def moveBetweenPackages = new FileSet {
    """
      package a.b.c
      class ToMove
    """ becomes
    """
      package x.y
      class ToMove
    """
  } applyRefactoring(moveTo("x.y"))
  
  @Test
  def moveObjectBetweenPackages = new FileSet {
    """
      package a.b.c
      object ToMove
    """ becomes
    """
      package x.y
      object ToMove
    """
  } applyRefactoring(moveTo("x.y"))
  
  @Test
  def moveBetweenNestedPackages = new FileSet {
    """
      package a
      package b
      package c
      class ToMove
    """ becomes
    """
      package x.y
      class ToMove
    """
  } applyRefactoring(moveTo("x.y"))
  
  @Test
  def moveBetweenSubPackage = new FileSet {
    """
      package org.com
      package pkg
      class ToMove
    """ becomes
    """
      package org.com
      package other
      class ToMove
    """
  } applyRefactoring(moveTo("org.com.other"))
  
  @Test
  def moveToSuperPackage = new FileSet {
    """
      package org.com
      package pkg
      class ToMove
    """ becomes
    """
      package org.com
      class ToMove
    """
  } applyRefactoring(moveTo("org.com"))
  
  
  
  /* TODO: 
   * - Move single class when there are multiple classes
   * - Move all classes from a file
   * - Moved class has dependencies that need to be imported (new or existing)
   * - Moved class is used somewhere else (add/adjust import)
   * - package objects?!
   */ 
  
}
