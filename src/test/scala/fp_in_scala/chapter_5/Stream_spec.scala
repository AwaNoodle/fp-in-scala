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

  "forAll" should "return true if a predicate is true for all elements" in {
    longerStream.forAll(x => x < 6) shouldBe true
  }

  it should "return false if any of the elements fails the predicate" in {
    longerStream.forAll(x => x < 3) shouldBe false
  }

  it should "return true if the stream is empty" in {
    Stream.empty[Int].forAll(x => x < 6) shouldBe true
  }

  // 5.5
  "takeWhile_2" should "take stream items while a predicate is true" in {
    longerStream.takeWhile_2(x => x < 4).toList shouldBe List(1,2,3)
    longerStream.takeWhile_2(x => false) shouldBe Empty
    Stream.empty[Int].takeWhile_2(x => x < 4) shouldBe Empty
  }

  // 5.6
  "headOption" should "return the first element of this stream if it is nonempty" in {
    longerStream.headOption shouldBe Some(1)
  }

  it should "return None if the stream is empty" in {
    Stream.empty[Int].headOption shouldBe None
  }

  // 5.7
  "map" should "convert a stream of one type into a stream of another type" in {
    shortStream.map(_.toString).toList shouldBe List("1","2")
  }

  "filter" should "return a stream with elements that match the filter" in {
    longerStream.filter(_ % 2 == 0).toList shouldBe List(2,4)
  }

  "add" should "add an element to the end of the stream" in {
    shortStream.add(() => 5).toList shouldBe List(1,2,5)
  }

  "append" should "append one stream to the other" in {
    shortStream.append(longerStream).toList shouldBe List(1,2,1,2,3,4,5)
  }

  "flatMap" should "flatten and map a stream" in {
    shortStream.flatMap(n => {
      Stream((0 to n).map(x => x.toString): _*)
    }).toList shouldBe List("0", "1", "0", "1", "2")
  }

  // 5.14
  "startsWith" should "return true if the main stream starts with the supplied stream" in {
    val startStream = Stream(1,2,3)
    val comparitorStream = Stream(1,2)

    startStream.startsWith(comparitorStream) shouldBe true
  }

  it should "return false if the main stream doens't start with the supplied stream" in {
    val startStream = Stream(1,2,3)
    val comparitorStream = Stream(8,9)

    startStream.startsWith(comparitorStream) shouldBe false
  }

  // 5.15
  "tails" should "return a stream of suffixes of the input stream" in {
    val startStream = Stream(1,2,3)
    val expected = Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty)

    def deepToList[A](s: Stream[Stream[A]]) = s.toList.map(_.toList)

    val result = startStream.tails
    deepToList(result) shouldBe deepToList(expected)
  }
}
