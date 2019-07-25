package fp_in_scala.chapter_4

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Try

class myEither_spec extends FlatSpec with Matchers {
  import MyEitherOps._

  def mapping_f(a: Int): Double = a.toDouble
  def flatmapping_f(a: Int): MyEither[String, Double] = MyRight(a.toDouble)
  def map2_f(a: Int, b: Int): Double = 99.99
  def traverse_f(intString: String): MyEither[String, Int] = 
      Try(intString.toInt).toEither match { 
        case Right(x) => MyRight[String, Int](x)
        case Left(e) => MyLeft(s"Opps - ${intString}")
      }

  "mapping" should "convert via f if MyRight" in {
    MyRight[String, Int](2).map(mapping_f) shouldBe MyRight[String, Double](2.0)
  }

  it should "return left if MyLeft with updated types" in {
    MyLeft[String, Int]("Error").map(mapping_f) shouldBe MyLeft[String, Double]("Error")
  }

  "flatmapping MyEither" should "convert via f if MyRight" in {
    MyRight[String, Int](2).flatMap(flatmapping_f) shouldBe MyRight[String, Double](2.0)
  }

  it should "return left if MyLeft with updated types" in {
    MyLeft[String, Int]("Error").flatMap(flatmapping_f) shouldBe MyLeft[String, Double]("Error")
  }

  "orElse" should "return the original value if it's a MyRight" in {
    MyRight[String, Int](2).orElse(MyRight[String, Double](4.0)) shouldBe MyRight[String, Int](2)
  }

  it should "return the else if the original value is MyLeft" in {
    MyLeft[String, Int]("Error").orElse(MyRight[String, Double](4.0)) shouldBe MyRight[String, Double](4.0)
  }

  "map2" should "return the result of f if values are not MyLeft" in {
    MyRight[String, Int](1).map2(MyRight[String, Int](2))(map2_f) shouldBe MyRight[String, Double](99.99)
  }

  it should "return MyLeft if either of the values are MyLeft" in {
    MyRight[String, Int](1).map2(MyLeft[String, Int]("Error"))(map2_f) shouldBe MyLeft[String, Int]("Error")
    MyLeft[String, Int]("Error").map2(MyRight[String, Int](1))(map2_f) shouldBe MyLeft[String, Int]("Error")
  }

  // 4.7
  "sequence" should "combine a seq of MyEither into a single MyEither with the values" in {
    val rights = List(MyRight[String, Int](1), MyRight[String, Int](2), MyRight[String, Int](3))
    sequence[String, Int](rights) shouldBe MyRight(List(1,2,3))
  }

  it should "return a MyLeft for the first encountered error" in {
    val eithers = List(MyRight[String, Int](1), MyLeft[String, Int]("Opps"), MyRight[String, Int](3))
    sequence[String, Int](eithers) shouldBe MyLeft("Opps")
  }

  "traverse" should "map a list to an MyEither, returning MyRight of the mapped list if the mapping values was succesful" in {
    traverse(List("1","2","3"))(traverse_f) shouldBe MyRight(List(1,2,3))
  }

  it should "return the first Left if any of the list result in Left" in {
    traverse(List("1","b","3"))(traverse_f) shouldBe MyLeft("Opps - b")
    traverse(List("1","2","c"))(traverse_f) shouldBe MyLeft("Opps - c")
  }
}
