package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class flatmap_spec extends FlatSpec with Matchers {
  import flatmap._

  // 3.15
  "flatmap" should "flatten all the lists down" in {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)
    val c = List(7, 8, 9)
    val d = List(a, b, c)
    flatmap(d) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
}