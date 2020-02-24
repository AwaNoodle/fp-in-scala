package fp_in_scala.chapter_7

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

class Par_spec extends FlatSpec with Matchers {
  import Par._

  private def sleepingFuture[A](sleepMs: Long, unit: A): Par[A] = es => es.submit(() => {
    Thread.sleep(sleepMs)
    unit
  })

  "map2" should "map the result of two Pars" in {
    val parA = unit(10)
    val parB = unit('C')

    val result = map2(parA, parB) { (a,b) => s"$a-$b" }
    val es = Executors.newCachedThreadPool()
    result(es).get shouldBe "10-C"
  }

  it should "throw when the timeout is violated by parA" in {
    val parA: Par[Int] = sleepingFuture(1000, 10)
    val parB = unit('C')

    val result = map2(parA, parB) { (a,b) => s"$a-$b" }
    val es = Executors.newCachedThreadPool()

    assertThrows[TimeoutException] {
      result(es).get(100, TimeUnit.MILLISECONDS)
    }
  }

  it should "throw when the timeout is violated by parB" in {
    val parA: Par[Int] = sleepingFuture(1000, 10)
    val parB: Par[Char] = sleepingFuture(5000, 'C')

    val result = map2(parA, parB) { (a,b) => s"$a-$b" }
    val es = Executors.newCachedThreadPool()

    assertThrows[TimeoutException] {
      result(es).get(1500, TimeUnit.MILLISECONDS)
    }
  }

  "asyncF" should "let you convert an A => B function to an async A => B" in {
    val f = (a: Int) => a.toString

    val af = asyncF(f)(10)

    val es = Executors.newCachedThreadPool()
    af(es).get shouldBe "10"
  }
}
