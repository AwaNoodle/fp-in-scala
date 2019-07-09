package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class tree_spec extends FlatSpec with Matchers {
  import tree._

  val simpleTree = Branch(Leaf(1), Leaf(2)) // 3 nodes, max 2 deep
  val fullTree = Branch(simpleTree, Branch(simpleTree, Branch(Leaf(3), Leaf(4)))) // 11 nodes, max 4 deep

  "size" should "count the nodes in a tree" in {
    tree.size(simpleTree) shouldBe 3
    tree.size(fullTree) shouldBe 11
  }

  "maximum" should "find the largest value in the tree" in {
    maximum(simpleTree) shouldBe 2
    maximum(fullTree) shouldBe 4
  }

  "depth" should "find the max depth in the tree" in {
    depth(simpleTree) shouldBe 2
    depth(fullTree) shouldBe 4
  }

  "map" should "allow the tree nodes to be transformed" in {
    def addTwo(a: Int) = a + 2
    map(simpleTree)(addTwo) shouldBe Branch(Leaf(3), Leaf(4))
  }
}