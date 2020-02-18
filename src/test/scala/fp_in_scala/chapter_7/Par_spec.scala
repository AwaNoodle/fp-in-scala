package fp_in_scala.chapter_7

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

class Par_spec extends FlatSpec with Matchers {
  import Par._

  "map2" should "map the result of two Pars" in {
    val parA = unit(10)
    val parB = unit('C')

    val result = map2(parA, parB) { (a,b) => s"$a-$b" }
    val es = Executors.newCachedThreadPool()
    result(es).get shouldBe "10-C"
  }
}