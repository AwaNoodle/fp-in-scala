package fp_in_scala.chapter_7

import java.time.{Duration, Instant}
import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  // 7.1, 7.3 add timeout
  // Combines the results of two parallel computations with a binary function
  // def map2[A,B,C](l: Par[A], r: Par[B])(f: (A,B) => C) : Par[C] = ???
  def map2[A,B,C](l: Par[A], r: Par[B], timeoutInMs: Int = 100)(f: (A,B) => C) : Par[C] = {
    (es: ExecutorService) => {
      val a = l(es)
      val b = r(es)
      //UnitFuture(f(a.get, b.get))

      // Ex 7.3
      map2Future(a, b, f, timeoutInMs)
    }
  }

  // From listing 7.4

  // Marks a computation for concurrent evaluation by run
  def fork[A](a: => Par[A]): Par[A] = {
    (es: ExecutorService) => {
      es.submit(new Callable[A] {
        def call = a(es).get
      })
    }
  }

  // Creates a computation that immediately results in the value a
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  
  // Wraps the expression a for concurrent evaluation by run
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  // Fully evaluates a given Par, spawning parallel computations as requested 
  // by fork and extracting the resulting value
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // From listing 7.5
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // Ex 7.3
  private def map2Future[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C, timeoutInMs: Int = 100): Future[C] = {
    val starts = Instant.now();
    val a = af.get(timeoutInMs, TimeUnit.MILLISECONDS)
    val ends = Instant.now()
    val remaining = timeoutInMs - Duration.between(starts, ends).toMillis()
    val b = bf.get(remaining, TimeUnit.MILLISECONDS)
    UnitFuture(f(a,b))
  }
}

