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

  // 6.6
  "map2" should "take two actions and combine them using f" in {
    val rng = TestRNG(Int.MaxValue /2)
    val action = map2(double, double){ (a,b) => (a, b)}

    action(rng) shouldBe ((0.4999999997438863, 0.4999999997438863), rng)
  }

  // 6.7
  "Sequence" should "take a list of Rands and return a list of results" in {
    val rng = TestRNG(Int.MaxValue /2)
    val listOfActions = (1 to 10).map(_ => double).toList
    val expectedResults = (1 to 10).map(_ => 0.4999999997438863)
    val seqAction = sequence(listOfActions)

    seqAction(rng) shouldBe (expectedResults, rng)
  }

  // 6.8
  "flatMap" should "allow a conversion of the result of the initial function via f" in {
    val rng = TestRNG(10)

    val action = flatMap(rng => rng.nextInt)(a => rng => (a.toDouble, rng))
    action(rng) shouldBe (10, rng)
  }

  // 6.11
  "Unit" should "wrap a value" in {
    val rng = TestRNG(10)
    unit(5)(rng) shouldBe (5,rng)
  }
}