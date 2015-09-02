package scala.tools.refactoring
package tests.util

import scala.util.Random
import scala.tools.refactoring.util.UnionFind
import org.junit.Test
import org.junit.Assert._
import org.junit.Before

class UnionFindTest {

  // We test this on 100 randomly colored Nodes
  val colorString = Array("Red", "Blue", "Green", "Yellow", "Blue")
  class Node(val color: Int){override def toString() = this.hashCode().toString() + "( " + colorString(color) + ")"}
  val testNodes: List[Node] = List.fill(100){new Node(Random.nextInt(5))}
  val uf = new UnionFind[this.Node]()

  @Before
  def unknownNodesShouldNotThrowWhenUnited() = {
	  for (node1 <- testNodes;
	    node2 <- testNodes if node1.color == node2.color) uf.union(node1, node2)
  }

  @Test
  def atMostFiveRepsInUF() = {
    val nodesFromUF = testNodes.map(uf.find(_)).distinct
    val repsLength = nodesFromUF.length
    assertTrue(s"Expected five reps in the union-find, found $repsLength !", repsLength <= 5)
  }

  @Test
  def nodesInRelationIfAndOnlyIfWithSameColor(){
    def sameColorImpliesRelation(x: Node, y: Node) = x.color != y.color || uf.find(x) == uf.find(y)
    def relationImpliesSameColor(x: Node, y: Node) = uf.find(x) != uf.find(y) || x.color == y.color
    for (x <- testNodes;
         y <- testNodes) {
      val px = uf.find(x)
      val py = uf.find(y)
      assertTrue(s"problem found with node $x (parent $px) and $y (parent $py)", sameColorImpliesRelation(x, y) && relationImpliesSameColor(x,y))
    }
  }

  def colorRepresentant(): Array[Node] = {
    val representants = new Array[Node](5)
    for (c <- 0 to 4){
      representants(c) = testNodes.collectFirst{ case (x: Node) if (x.color == c) => uf.find(x)}.getOrElse(uf.find(new Node(c)))
    }
    representants
  }

  @Test
  def classRepresentantIsUnique(){
    val reps = colorRepresentant()
    testNodes.foreach{(x) => {
	val xColor = colorString(x.color)
	val xColorRep = reps(x.color)
        assertTrue(s"problem found with $x yet the representant of $xColor is $xColorRep", uf.find(x) == xColorRep)}
    }
  }

  @Test
  def findIsIdempotent(){
    assertTrue(testNodes.forall{(x) => val p = uf.find(x); p == uf.find(p)})
  }

  @Test
  def nodesForWhichFindIsIdentityAreReps(){
    val selfRepresented = testNodes.filter{ (n)=> uf.find(n) == n }
    val reps = colorRepresentant()
    assertTrue(reps.forall{(x) => selfRepresented.contains(x)})
    assertTrue(selfRepresented.forall{(x) => reps.contains(x)})
  }

  @Test
  def equivalenceClassGivesAColor(){
	  def myClassIsExactlyMyColor(n: Node): Boolean = {
	    val myClass = uf.equivalenceClass(n)
	    val inClassImpliesSameColor = testNodes.forall{ (x) => !myClass.contains(x) || x.color == n.color}
	    val sameColorImpliesInclass = testNodes.forall{ (x) => x.color != n.color || myClass.contains(x)}
	    inClassImpliesSameColor && sameColorImpliesInclass
	  }
	  // A bit overkill to do this on more than representants
	  assertTrue(colorRepresentant().forall{myClassIsExactlyMyColor})
  }

}
