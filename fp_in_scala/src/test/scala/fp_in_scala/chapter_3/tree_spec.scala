package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class tree_spec extends FlatSpec with Matchers {
  import tree._

  val simpleTree = Branch(Leaf(1), Leaf(20)) // 3 nodes, max 2 deep
  val fullTree = Branch(simpleTree, Branch(simpleTree, Branch(Leaf(3), Leaf(40)))) // 11 nodes, max 4 deep

  "size" should "count the nodes in a tree" in {
    tree.size(simpleTree) shouldBe 3
    tree.size(fullTree) shouldBe 11
  }

  "maximum" should "find the largest value in the tree" in {
    maximum(simpleTree) shouldBe 20
    maximum(fullTree) shouldBe 40
  }

  "depth" should "find the max depth in the tree" in {
    depth(simpleTree) shouldBe 2
    depth(fullTree) shouldBe 4
  }

  "map" should "allow the tree nodes to be transformed" in {
    def addTwo(a: Int) = a + 2
    map(simpleTree)(addTwo) shouldBe Branch(Leaf(3), Leaf(22))
  }

  "fold" should "allow for size to be implemented" in {
    def f(a: Int) = 1 // Leaf is 1 as we're just adding size
    def g(x: Int, y: Int) = x + y + 1 // Branch addition

    fold(simpleTree)(f)(g) shouldBe 3
    fold(fullTree)(f)(g) shouldBe 11
  }

  it should "allow maximum to be implemented" in {
    def f(a: Int) = a
    def g(x: Int, y: Int) = x max y

    fold(simpleTree)(f)(g) shouldBe 20
    fold(fullTree)(f)(g) shouldBe 40
  }

  it should "allow depth to be implemented" in {
    def f(a: Int) = 1
    def g(x: Int, y: Int) = 1 + (x max y)

    fold(simpleTree)(f)(g) shouldBe 2
    fold(fullTree)(f)(g) shouldBe 4
  }

  it should "allow map to be implemented" in {
    def f(a: Int): Tree[Int] = Leaf(a + 2)
    def g(x: Tree[Int], y: Tree[Int]): Tree[Int] = Branch(x, y)
    
    fold(simpleTree)(f)(g) shouldBe Branch(Leaf(3), Leaf(22))
  }
}