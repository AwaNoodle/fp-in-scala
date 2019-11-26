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

  "Sequence" should "take a list of States and return a list of results" in {
    val states = (1 to 10).map(x => State[String, Int](s => (x, s))).toList
    val expected = ((1 to 10).toList, "State")
    
    State.sequence(states).run("State") shouldBe expected
  }
}
