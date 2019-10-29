package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class RNGOps_spec extends FlatSpec with Matchers {
  import RNGOps._

  case class TestRNG(testValue: Int) extends RNG {
    def nextInt: (Int, RNG) = (testValue, this)
  }

  "nonNegativeInt" should "return an int between 0 and maxValue" in {
    val someValue = TestRNG(10)
    nonNegativeInt(someValue) shouldBe (10, someValue)
  }

  it should "return the abs value if the random number is less than 0" in {
    val someValue = TestRNG(-50)
    nonNegativeInt(someValue) shouldBe (50, someValue)
  }

  it should "return Int.MaxValue if Int.MinValue is returned by the random generator" in {
    val someValue = TestRNG(Int.MinValue)
    nonNegativeInt(someValue) shouldBe (Int.MaxValue, someValue)
  }
}