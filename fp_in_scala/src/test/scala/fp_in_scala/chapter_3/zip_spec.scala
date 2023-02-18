package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class zip_spec extends FlatSpec with Matchers {
  import zip._

  // 3.22
  "zipWith" should "zip while applying the higher-order function" in {
    val a = List(1,2,3)
    val b = List(4,5,6)
    zipWith(a,b)(_ + _) shouldBe List(5,7,9)
  }
}