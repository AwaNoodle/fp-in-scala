package fp_in_scala.chapter_7

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

class NonBlockingPar_spec extends FlatSpec with Matchers {
  import NonBlockingPar._

  private def runPar[A] = {
    val es = Executors.newCachedThreadPool()
    NonBlockingPar.run[A](es)(_)
  }

  "NonBlockingPar" should "run the passed in function" in {
    val sut = NonBlockingPar.unit(5)
    runPar(sut) shouldBe 5
  }
}
