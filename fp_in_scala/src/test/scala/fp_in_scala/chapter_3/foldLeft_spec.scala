package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class foldLeft_spec extends FlatSpec with Matchers {
  import foldLeft._

  val testIntList = List(1, 2, 3, 4)

  // 3.11
  "foldLeft" should "sum the list" in {
    def sum(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

    sum(testIntList) shouldBe 10
  }

  it should "return the product of the list" in {
    def product(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)

    product(testIntList) shouldBe 24
  }

  it should "return the length of the list" in {
    def length[A](as: List[A]): Int = foldLeft(testIntList, 0)((a, _) => a + 1)

    length(testIntList) shouldBe 4
  }

  // 3.12
  it should "reverse the list" in {
    def reverseList(lst: List[Int]) = foldLeft(lst, List[Int]())((b, a) => a +: b)
    reverseList(testIntList) shouldBe List(4,3,2,1)
  }
}
