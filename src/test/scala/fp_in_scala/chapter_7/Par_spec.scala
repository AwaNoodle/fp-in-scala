package fp_in_scala.chapter_7

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

class Par_spec extends FlatSpec with Matchers {
  import Par._

  private def runPar[A] = {
    val es = Executors.newCachedThreadPool()
    Par.run[A](es)(_)
  }

  private def sleepingFuture[A](sleepMs: Long, unit: A): Par[A] = es => es.submit(() => {
    Thread.sleep(sleepMs)
    unit
  })

  "map2" should "map the result of two Pars" in {
    val parA = unit(10)
    val parB = unit('C')

    val result = map2(parA, parB) { (a,b) => s"$a-$b" }

    runPar(result).get shouldBe "10-C"
  }

  it should "throw when the timeout is violated by parA" in {
    val parA: Par[Int] = sleepingFuture(1000, 10)
    val parB = unit('C')
    val result = map2(parA, parB) { (a,b) => s"$a-$b" }

    assertThrows[TimeoutException] {
      runPar(result).get(100, TimeUnit.MILLISECONDS)
    }
  }

  it should "throw when the timeout is violated by parB" in {
    val parA: Par[Int] = sleepingFuture(1000, 10)
    val parB: Par[Char] = sleepingFuture(5000, 'C')
    val result = map2(parA, parB) { (a,b) => s"$a-$b" }

    assertThrows[TimeoutException] {
      runPar(result).get(1500, TimeUnit.MILLISECONDS)
    }
  }

  "asyncF" should "let you convert an A => B function to an async A => B" in {
    val f = (a: Int) => a.toString
    val af = asyncF(f)(10)

    runPar(af).get shouldBe "10"
  }

  "sequence" should "take a list of List[Par[A]] and change this to a Par List of A" in {
    val parList = List(unit(10), unit(20), unit(30))
    val expected = List(10,20,30)
    val actualPar = sequence(parList)

    runPar(actualPar).get shouldBe expected
  }

  "parFilter" should "filter a list in parallel" in {
    val items = List(1,2,3,4,5,6)
    val expected = List(2,4,6)

    val result = parFilter(items){ _ % 2 == 0 }
    runPar(result).get shouldBe expected
  }
}
