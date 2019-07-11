package fp_in_scala.chapter_4

import org.scalatest.{FlatSpec, Matchers}

class mvariance_spec extends FlatSpec with Matchers {
  import variance._

  val simpleTest = Seq(1.0,2.0,3.0,4.0,5.0) // mean 3, variance = 2

  "mean" should "calculate the mean and return a Some with the value" in {
    mean(simpleTest) shouldBe Some(3.0)
  }

  it should "return None for an empty Seq" in {
    mean(Seq[Double]()) shouldBe None
  }
  
  "variance" should "show the variance of the elements in a sequence" in {
    variance(simpleTest) shouldBe Some(2.0)
  }

  it should "return None if the seq is empty" in {
    variance(Seq[Double]()) shouldBe None
  }
}
