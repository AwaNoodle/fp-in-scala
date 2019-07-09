package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class mapping_spec extends FlatSpec with Matchers {
  import mapping._

  val intList = List(1,2,3,4)
  val doubleList = List[Double](0.0, 0.1, 0.2, 0.3)
  def isEven(a: Int) = a % 2 == 0
 
  "map" should "transform a list using a higher-order function" in {
     // 3.16
    def addOne(a: Int): Int = a + 1
    map(intList)(addOne) shouldBe List(2,3,4,5)

    // 3.17
    def toString(a: Double): String = a.toString
    map(doubleList)(toString) shouldBe List("0.0", "0.1", "0.2", "0.3")
  }

  "filter" should "filter lists using a predicate" in {
    // 3.19
    
    filter(intList)(isEven) shouldBe List(2,4)
  }

  "flatMap" should "flatten and map using a higher order function" in {
    //3.20
    flatMap(intList)(i => List(i,i)) shouldBe List(1,1,2,2,3,3,4,4)
  }

  "filter_2" should "filter lists using a predicate" in {
    // 3.21
    filter_2(intList)(isEven) shouldBe List(2,4)
  }
}
