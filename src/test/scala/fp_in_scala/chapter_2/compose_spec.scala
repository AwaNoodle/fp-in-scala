package fp_in_scala.chapter_2

import org.scalatest.{FlatSpec, Matchers}

class compose_spec extends FlatSpec with Matchers {
  import compose._

  "compose" should "give the result of the second func fed into the first" in {
    val composeFunc = compose((a: Int) => a + 10, (b: Int) => b + 20)

    composeFunc(5) shouldBe 35
  }
}
