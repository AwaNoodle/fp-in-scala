package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class tail_spec extends FlatSpec with Matchers {
  import tail._

  val test = List(1,2,3,4,5,6)
  val headTest = List("b", "c", "d")

  "tail" should "return the tail of a list" in {
    tail(test) shouldBe List(2,3,4,5,6)
    tail(Nil) shouldBe Nil
  }

  "setHead" should "replace the head of a list" in {
    setHead("a", headTest) shouldBe List("a", "c", "d")
  }

  "drop" should "remove n elements from the start of a list" in {
    drop(test, 2) shouldBe List(3,4,5,6)
    drop(test, 4) shouldBe List(5,6)
  }

  "dropWhile" should "remove elements where the predicate is true" in {
    def isDivisableByTwo(x: Int): Boolean = (x % 2) == 0
    dropWhile(test, isDivisableByTwo) shouldBe List(1,3,5)
  }

  "init" should "return all but the last element in a list" in {
    val newList = init(test)
    newList shouldBe List(1,2,3,4,5)
  }
}
