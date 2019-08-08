package fp_in_scala.chapter_5

import org.scalatest.{FlatSpec, Matchers}

class Infinites_spec extends FlatSpec with Matchers {
  import Infinites._
  
  // 5.8
  "constant" should "produce an infinate stream of type A" in {
    val constant_a = constant("a")
    constant_a.take(5).toList shouldBe List("a","a","a","a","a")
    constant_a.take(20).toList shouldBe (1 to 20).map(_ => "a").toList

    val constant_ten = constant(10)
    constant_ten.take(107).toList shouldBe (1 to 107).map(_ => 10).toList
  }

  // 5.9
  "from" should "produce a stream of incrementing ints" in {
    from(5).take(5).toList shouldBe List(5,6,7,8,9)
    from(1).take(1000).toList shouldBe (1 to 1000).toList
  }
}