package fp_in_scala.chapter_2

import org.scalatest.{FlatSpec, Matchers}

class isSorted_spec extends FlatSpec with Matchers {
  import isSorted._

  "isSorted" should "show an empty seq is sorted" in {
    isSorted(Seq[Int](), comparitor) shouldBe true
  }

  it should "show a seq of 1 is sorted" in {
    isSorted(Seq[Int](1), comparitor) shouldBe true
  }

  it should "show An ordered seq is sorted" in {
    isSorted(Seq[Int](1, 2, 3), comparitor) shouldBe true
  }

  it should "show an unordered seq is not sorted" in {
    isSorted(Seq[Int](1, 3, 2), comparitor) shouldBe false
  }
}
