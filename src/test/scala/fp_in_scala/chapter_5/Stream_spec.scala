package fp_in_scala.chapter_5

import org.scalatest.{FlatSpec, Matchers}

class Stream_spec extends FlatSpec with Matchers {
  val shortStream = Stream.cons[Int](1, Stream.cons(2, Empty))
  val longerStream = Seq(1,2,3,4,5).foldRight[Stream[Int]](Empty)((x,b) => Stream.cons(x, b))

  // 5.1
  "toList" should "convert a stream to a list" in {
    shortStream.toList shouldBe List(1,2)
    longerStream.toList shouldBe List(1,2,3,4,5)
  }

  it should "return Nil for an empty stream" in {
    Empty.toList shouldBe Nil
  }

  // 5.2
  "take" should "take n amount of values from a stream if available" in {
    shortStream.take(1) shouldBe List(1)
    longerStream.take(5) shouldBe List(1,2,3,4,5)
  }

  it should "return a truncated list if not enough values are available" in {
    shortStream.take(5) shouldBe List(1,2)
    longerStream.take(10) shouldBe List(1,2,3,4,5)
  }

  it should "return an empty list for an empty stream" in {
    Empty.take(10) shouldBe Nil
  }

  it should "return an empty list for negative or zero values of n" in {
    shortStream.take(0) shouldBe Nil
    shortStream.take(-1) shouldBe Nil
  }
}
