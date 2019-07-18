package fp_in_scala.chapter_4

import org.scalatest.{FlatSpec, Matchers}

class lifting_spec extends FlatSpec with Matchers {
  import lifting._

  def f(a: String, b: Int): Double = 99.99
  
  "map2" should "return the result of f if values are not None" in {
    map2(Some("Hello"), Some(123))(f) shouldBe Some(99.99)
  }

  it should "return None if either of the values are None" in {
    map2(None, Some(123))(f) shouldBe None
    map2(Some("Hello"), None)(f) shouldBe None
  }

  "sequence" should "combine a list into a single option of the list of values" in {
    val options = List(
      Some(1),
      Some(2),
      Some(3)
    )

    sequence(options) shouldBe Some(List(1,2,3))
  }

  it should "return None if the list contains None" in {
    val options = List(
      Some(1),
      None,
      Some(3)
    )

    sequence(options) shouldBe None
  }
}
