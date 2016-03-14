package scala.tools.refactoring
package tests.util

import scala.util.Random
import scala.tools.refactoring.util.UnionFind
import org.junit.Test
import org.junit.Assert._

class UnionFindInitTest {

  // We test this on 100 randomly colored Nodes
  val colorString = Array("Red", "Blue", "Green", "Yellow", "Blue")
  class Node(val color: Int){override def toString() = this.hashCode().toString() + "( " + colorString(color) + ")"}
  val testNodes: List[Node] = List.fill(100){new Node(Random.nextInt(5))}
  val uf = new UnionFind[this.Node]()

  @Test
  def firstInsertedNodesShouldBeTheirOwnParents() = {
    for (node <- testNodes) uf.find(node)
    // inserted Nodes are their Parents
    assertTrue(testNodes.forall{(x) => uf.find(x) == x})
  }
}
