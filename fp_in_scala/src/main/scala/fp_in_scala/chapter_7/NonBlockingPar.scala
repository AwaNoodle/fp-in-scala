package fp_in_scala.chapter_7

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

object NonBlockingPar {
  sealed trait Future[A] {
    private[chapter_7] def apply(k: A => Unit): Unit
  }

  type NonBlockingPar[A] = ExecutorService => Future[A]

  def unit[A](a: A): NonBlockingPar[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def run[A](es: ExecutorService)(p: NonBlockingPar[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown()}

    latch.await

    ref.get
  }
}
