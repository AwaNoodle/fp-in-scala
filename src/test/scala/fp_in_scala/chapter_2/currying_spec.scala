package fp_in_scala.chapter_2

import org.scalatest.{FlatSpec, Matchers}

class currying_spec extends FlatSpec with Matchers {
  import currying._

  "currying" should "curry the supplied function so you can supply params in different steps" in {
    val first = currying((a: Int, b: Int) => a + b)
    val second = first(5)
    second(10) shouldBe 15
  }

  "uncurrying" should "merge the params together" in {
    val uncurried = uncurry((a:Int) => (b: Int) => a + b)
    uncurried(5, 10) shouldBe 15
  }
}
