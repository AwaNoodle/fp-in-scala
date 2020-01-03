package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class State_spec extends FlatSpec with Matchers {
  import State._

  "unit" should "wrap a value" in {
    State.pure(10).run("State") shouldBe (10, "State")
  }

  "flatMap" should "allow a conversion of the result of the initial function via f" in {
    State.pure(10)
         .flatMap(a => State.pure[String, Double](a.toDouble))
         .run("State") shouldBe (10.0, "State")
  }

  "map" should "allow a conversion of the result of the initial function via f" in {
    State.pure(10)
         .map(a => a.toDouble)
         .run("State") shouldBe (10.0, "State")
  }

  "map2" should "allow the combination of two states" in {
    val a = State.pure[String, Int](10)
    val b = State.pure[String, String]("b")

    State.map2(a,b) { (a,b) => (a,b)}.run("Hello") shouldBe ((10, "b"), "Hello")
  }

  "Sequence" should "take a list of States and return a list of results" in {
    val states = (1 to 10).map(x => State[String, Int](s => (x, s))).toList
    val expected = ((1 to 10).toList, "State")
    
    State.sequence(states).run("State") shouldBe expected
  }
}
