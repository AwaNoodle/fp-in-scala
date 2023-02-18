package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class foldRight_spec extends FlatSpec with Matchers {
  import foldRight._

  val testIntList = List(1, 2, 3, 4)
  val testDoubleList = List(1.0, 2.0, 3.0, 4.0)

  "foldRight" should "sum the list" in {
    sum(testIntList) shouldBe 10
  }

  it should "return the product of the list" in {
    product(testDoubleList) shouldBe 24.0
  }

  it should "return the length of the list" in {
    fp_in_scala.chapter_3.foldRight.length(testIntList) shouldBe 4
  }

  it should "reverse the list" in {
    reverseList(testIntList) shouldBe List(4,3,2,1)
  }

  it should "append an item to a list" in {
    append(testIntList, 5) shouldBe List(1,2,3,4,5)
  }
}
