package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class RNGOps_spec extends FlatSpec with Matchers {
  import RNGOps._

  case class TestRNG(testValue: Int) extends RNG {
    def nextInt: (Int, RNG) = (testValue, this)
  }

  // 6.1
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

  // 6.2
  "double" should "generate a random number between 0 and 1" in {
    val someValue = TestRNG(Int.MaxValue / 2)
    double(someValue) shouldBe (0.4999999997438863, someValue)
  }

  it should "not return 1" in {
    val someValue = TestRNG(Int.MaxValue)
    double(someValue) shouldBe (0.9999999999534339, someValue)
  }

  // 6.3
  "intDouble" should "return a random Int,Double pair" in {
    val halfMaxValue = Int.MaxValue / 2
    val someValue = TestRNG(halfMaxValue)
    intDouble(someValue) shouldBe ((halfMaxValue, 0.4999999997438863), someValue)
  }

  "doubleInt" should "return a random Double,Int pair" in {
    val halfMaxValue = Int.MaxValue / 2
    val someValue = TestRNG(halfMaxValue)
    doubleInt(someValue) shouldBe ((0.4999999997438863, halfMaxValue), someValue)
  }

  "double3" should "return a tuple3 of random doubles" in {
    val halfMaxValue = Int.MaxValue / 2
    val expected = 0.4999999997438863
    val someValue = TestRNG(halfMaxValue)

    double3(someValue) shouldBe ((expected, expected, expected), someValue)
  }

  // 6.4
  "ints" should "return a list of random ints of the desired size" in {
    val rng = TestRNG(50)
    val expectedLength = 10
    val expected = ((1 to expectedLength).map(x => 50), rng)
    
    val randomInts = ints(10)(rng) 
    randomInts._1.length shouldBe expectedLength
    randomInts shouldBe expected
  }
}