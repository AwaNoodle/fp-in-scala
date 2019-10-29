package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class SimpleRNG_spec extends FlatSpec with Matchers {
  import RNGOps._

  case class TestRNG(testValue: Int) extends RNG {
    def nextInt: (Int, RNG) = (testValue, this)
  }

  "nonNegativeInt" should "return an int between 0 and maxValue" in {
    val someValue = TestRNG(10)
    nonNegativeInt(someValue) shouldBe (10, someValue)
  }
}