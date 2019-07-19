package fp_in_scala.chapter_4

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Try

class lifting_spec extends FlatSpec with Matchers {
  import lifting._

  def map2_f(a: String, b: Int): Double = 99.99
  def traverse_f(a: String): Option[Int] = Try(a.toInt).toOption
  
  "map2" should "return the result of f if values are not None" in {
    map2(Some("Hello"), Some(123))(map2_f) shouldBe Some(99.99)
  }

  it should "return None if either of the values are None" in {
    map2(None, Some(123))(map2_f) shouldBe None
    map2(Some("Hello"), None)(map2_f) shouldBe None
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

  "traverse" should "map a list to and option type, returning Some of the mapped list if the mapping values was succesful" in {
    traverse(List("1","2","3"))(traverse_f) shouldBe Some(List(1,2,3))
  }

  it should "return None if any of the list result in None after being passed to f" in {
    traverse(List("1","b","3"))(traverse_f) shouldBe None
    traverse(List("1","2","b"))(traverse_f) shouldBe None
  }
}
