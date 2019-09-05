package fp_in_scala.chapter_5

import org.scalatest.{FlatSpec, Matchers}

class Infinites_spec extends FlatSpec with Matchers {
  import Infinites._
  
  // 5.8
  "constant" should "produce an infinite stream of type A" in {
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

  // 5.10
  "fibs" should "produce a stream of Fibbonaci numbers" in {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  // 5.11
  "unfold" should "produce a stream from the supplied function" in { 
    def inc(currState: Int): Option[(Int, Int)] = {
      Some((currState, currState + 1))
    }

    unfold(0)(inc).take(7).toList shouldBe (0 to 6).toList
  }

  it should "finish the stream when the func returns None" in {
    def inc(currState: Int): Option[(Int, Int)] = {
      if(currState < 5) {
        Some((currState, currState + 1))
      } else {
        None
      }
    }

    unfold(0)(inc).take(7).toList shouldBe (0 to 4).toList
  }

  // 5.12
  it should "allow an implementation of fibs" in {
    // Pair is previous, current
    def fibs(s: (Int, Int)): Option[(Int, (Int, Int))] = {    
      val current = if(s._2 == 0) 1 else s._2
      Some((s._2, (s._2, s._1 + current)))
    }

    unfold((0,0))(fibs)
      .take(7)
      .toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "allow an implementation of from" in {
    def from(current: Int): Option[(Int,Int)] = Some(current -> (current + 1))

    unfold(5)(from).take(5).toList shouldBe List(5,6,7,8,9)
    unfold(1)(from).take(1000).toList shouldBe (1 to 1000).toList
  }

  it should "allow an implementation of constant" in {
    def constant[T](value: T): Option[(T, T)] = Some(value -> value)

    unfold("a")(constant).take(5).toList shouldBe List("a","a","a","a","a")
    unfold(10)(constant).take(107).toList shouldBe (1 to 107).map(_ => 10).toList
  }

  it should "allow an implementation of ones" in {
    def ones(value: Int): Option[(Int, Int)] = Some(value -> value)

    unfold(1)(ones).take(10) shouldBe (1 to 10).map(_ => 1)
  }
}
