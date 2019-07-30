package fp_in_scala.chapter_5

import org.scalatest.{FlatSpec, Matchers}

class Stream_spec extends FlatSpec with Matchers {
  val shortStream = Stream(1, 2)
  val longerStream = Stream(1,2,3,4,5) 

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

  "drop" should "drop n and return the rest of the stream" in {
    longerStream.drop(2).toList shouldBe List(3,4,5)
  }

  it should "return the whole stream if n is 0 or lower" in {
    longerStream.drop(0).toList shouldBe List(1,2,3,4,5)
    longerStream.drop(-5).toList shouldBe List(1,2,3,4,5)
  }

  it should "return an empty stream if you drop more than the stream contains" in {
    longerStream.drop(10) shouldBe Empty
  }

  it should "return empty for an empty stream" in {
    Empty.drop(10) shouldBe Empty
  }

  // 5.3
  "takeWhile" should "take stream items while a predicate is true" in {
    longerStream.takeWhile(x => x < 4).toList shouldBe List(1,2,3)
    longerStream.takeWhile(x => false) shouldBe Empty
    Stream.empty[Int].takeWhile(x => x < 4) shouldBe Empty
  }
}
