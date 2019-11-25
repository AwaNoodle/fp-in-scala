package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class State_spec extends FlatSpec with Matchers {
  import State._

  "unit" should "wrap a value" in {
    State.unit(10).run("State") shouldBe (10, "State")
  }

  "flatMap" should "allow a conversion of the result of the initial function via f" in {
    State.unit(10)
         .flatMap(a => State.unit[String, Double](a.toDouble))
         .run("State") shouldBe (10.0, "State")
  }

  "map" should "allow a conversion of the result of the initial function via f" in {
    State.unit(10)
         .map(a => a.toDouble)
         .run("State") shouldBe (10.0, "State")
  }
}
