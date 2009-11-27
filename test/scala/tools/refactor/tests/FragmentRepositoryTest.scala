package scala.tools.refactor.tests

import utils.TestHelper
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import scala.tools.refactor.printer._
import scala.tools.nsc.ast.Trees

@Test
class FragmentRepositoryTest extends TestHelper {
  
  import compiler.Tree
      
  class Whitespace(val print: String) extends Fragment {
    override val isWhitespace = true
  }
  
  val scope = new SimpleScope(None, 2) 
  
  val c1 = new StringFragment("aa"); scope.add(c1)
  val c2 = new Whitespace    (" {"); scope.add(c2)
  val c3 = new StringFragment("bb"); scope.add(c3)
  val c4 = new Whitespace    (" }"); scope.add(c4)
  val c5 = new Whitespace    (" ;"); scope.add(c5)
  val c6 = new StringFragment("cc"); scope.add(c6)
  
  val root = new SimpleScope(None, 0)
  root.add(new Whitespace    ("("))
  root.add(scope)
  root.add(new Whitespace    (")"))
  
  val fs = new FragmentRepository(root)

  @Test
  def indentation() = {
    assertEquals( Some(2), fs.scopeIndentation(c1))
    assertEquals( Some(0), fs.scopeIndentation(root.children.head))
    assertEquals( Some(0), fs.scopeIndentation(scope))
    assertEquals( None,    fs.scopeIndentation(new StringFragment("xx")))
  }
  
  @Test
  def invalidFragments() = {
    assertEquals( None, fs.getNext(new StringFragment("xx")))
    assertEquals( None, fs.getPrevious(new StringFragment("xx")))
  }  

  @Test
  def endOfScope() = {
    assertEquals( Some(c6, Nil, scope.children.last), fs.getNext(c6))
  }   
  
  @Test
  def beginOfScope() = {
    assertEquals( Some(scope.children.head, Nil, c1), fs.getPrevious(c1))
  }  
  
  @Test
  def previousInSimpleScope() = {
    assertEquals( Some(c1, c2 :: Nil, c3), fs.getPrevious(c3))
  }  
  
  @Test
  def nextInSimpleScope() = {
    assertEquals( Some(c3, c4 :: c5 :: Nil, c6), fs.getNext(c3))
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
    assertFalse(fs exists new Whitespace    (" ;"))
  }
  
  //@Test
  def testScopeIndentation() = {
    val tree = treeFrom("class A")
    val fs = new FragmentRepository(splitIntoFragments(tree))

    def collectAll(t: Tree): List[Tree] = {
      
      def children(t: Tree): List[Tree] = compiler.treeBrowsers.TreeInfo.children(t) filter (_.pos.isRange)
      
      t :: (children(t) flatMap ( child => collectAll(child)))
    }
    
    collectAll(tree) map ( t => (t, fs scopeIndentation t) ) foreach {
      case (t, indentation) => assertTrue("Indentation for tree "+ t +" not found.", indentation.isDefined)
    }
  }
}

