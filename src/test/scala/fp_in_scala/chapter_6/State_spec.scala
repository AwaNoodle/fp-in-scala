package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class State_spec extends FlatSpec with Matchers {
  import State._

  "unit" should "wrap a value" in {
    State.unit(10).run("State") shouldBe (10, "State")
  }
}