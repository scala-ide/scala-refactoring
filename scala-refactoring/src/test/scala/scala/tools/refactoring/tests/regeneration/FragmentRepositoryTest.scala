/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring.tests.regeneration

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactoring.regeneration._
import scala.tools.nsc.ast.Trees

@Test
class FragmentRepositoryTest extends TestHelper {
  
  import global.Tree
      
  class Layout(val print: Seq[Char]) extends Fragment {
  }
  
  val scope = new SimpleScope(None, 2)
  
  val c1 = new OriginalSourceFragment {
    def start = throw new Exception("Not Implemented!")
    def end = throw new Exception("Not Implemented!")
    def file = throw new Exception("Not Implemented!")
    override def print = "aa"
  }; scope.add(c1)
  
  val c2 = new Layout    (" {"); scope.add(c2)
  
  val c3 = new OriginalSourceFragment {
    def start = throw new Exception("Not Implemented!")
    def end = throw new Exception("Not Implemented!")
    def file = throw new Exception("Not Implemented!")
    override def print = "bb"
  }; scope.add(c3)
  
  val c4 = new Layout    (" }"); scope.add(c4)
  val c5 = new Layout    (" ;"); scope.add(c5)
  
  val c6 = new OriginalSourceFragment {
    def start = throw new Exception("Not Implemented!")
    def end = throw new Exception("Not Implemented!")
    def file = throw new Exception("Not Implemented!")
    override def print = "cc"
  }; scope.add(c6)
  
  val root = new SimpleScope(None, 0)
  root.add(new Layout    ("("))
  root.add(scope)
  root.add(new Layout    (")"))
  
  val fs = new FragmentRepository(root)

  @Test
  def indentation() = {
    assertEquals(Some(2), fs.scopeIndentation(c1))
    assertEquals(Some(0), fs.scopeIndentation(root.children.head))
    assertEquals(Some(0), fs.scopeIndentation(scope))
    assertEquals(None,    fs.scopeIndentation(new StringFragment("xx")))
  }
  
  @Test
  def invalidFragments() = {
    assertEquals(None, fs.getNext(new StringFragment("xx")))
    assertEquals(None, fs.getPrevious(new StringFragment("xx")))
  }  
  
  @Test
  def exists() = {
    assertTrue(fs exists c1)
    assertTrue(fs exists c2)
    assertTrue(fs exists c3)
    assertTrue(fs exists c4)
    assertTrue(fs exists c5)
    assertTrue(fs exists c6)
  }  
  
  @Test
  def notExists() = {
    assertFalse(fs exists new StringFragment("xx"))
    assertFalse(fs exists new Layout    (" ;"))
  }
  
  //@Test FIXME
  def testScopeIndentation() = {
    val tree = treeFrom("class A")
    val fs = new FragmentRepository(splitIntoFragments(tree))

    def collectAll(t: Tree): List[Tree] = {
      
      def children(t: Tree): List[Tree] = global.treeBrowsers.TreeInfo.children(t) filter (t => t.pos.isRange && t != global.EmptyTree)
      
      t :: (children(t) flatMap ( child => collectAll(child)))
    }
    
    collectAll(tree) map ( t => (t, fs scopeIndentation t) ) foreach {
      case (t, Some(_)) => ()
      case (t, None) => fail("Indentation for tree "+ t +" not found.")
    }
  }
}

