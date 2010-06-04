/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.transformation

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import org.junit.Assert._
import scala.tools.refactoring.transformation._

@Test
class TransformTest extends TestHelper with Transform {

  @Test
  def testReplaceTrees() = {
    val ts = List(1, 2, 3, 4, 5)
    
    assertEquals(List(1, 6, 3, 4, 5),    ts.replaceSequence(2 :: Nil, 6 :: Nil))
    assertEquals(List(6, 2, 3, 4, 5),    ts.replaceSequence(1 :: Nil, 6 :: Nil))
    assertEquals(List(1, 2, 3, 4, 6, 7), ts.replaceSequence(5 :: Nil, 6 :: 7 :: Nil))
    assertEquals(List(1, 2, 3, 4, 5),    ts.replaceSequence(6 :: Nil, 1 :: Nil))
    
    assertEquals(List(2, 1, 1), List(1, 1, 1).replaceSequence(1 :: Nil, 2 :: Nil))
  }
}

