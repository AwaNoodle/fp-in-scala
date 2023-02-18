package fp_in_scala.chapter_3

import org.scalatest.{FlatSpec, Matchers}

class subsequence_spec extends FlatSpec with Matchers {
  import subsequence._

  val exampleList = (1 to 20).toList

  "hasSubsequence" should "return true if a subsequence appears in a list" in {
    hasSubsequence(exampleList, List(1,2,3,4)) shouldBe true
  }

  it should "return false if the subsequence doesn't appear" in {
    hasSubsequence(exampleList, List(1,4)) shouldBe false
  }

  it should "return false if the main list is shorter than the subsequence" in {
    hasSubsequence(List(1,2), List(1,2,3,4))
  }
}