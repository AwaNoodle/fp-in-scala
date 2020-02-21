package fp_in_scala.chapter_7

import java.time.{Duration, Instant}
import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  // 7.1, 7.3 use Map2Future
  // Combines the results of two parallel computations with a binary function
  def map2[A,B,C](l: Par[A], r: Par[B])(f: (A,B) => C) : Par[C] = {
    (es: ExecutorService) => {
      val a = l(es)
      val b = r(es)
      //UnitFuture(f(a.get, b.get))

      // Ex 7.3
      new Map2Future(a, b, f)
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

  // Oli suggestion on Ex 7.3 (signature)
  private class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C)  extends Future[C] {
    def isDone = true
    def get = f(fa.get, fb.get)
    def get(timeout: Long, units: TimeUnit) = { 
      val starts = Instant.now();
      val a = fa.get(timeout, units)
      val ends = Instant.now()
      val remaining = units.toMillis(timeout) - Duration.between(starts, ends).toMillis()
      val b = fb.get(remaining, TimeUnit.MILLISECONDS)
      f(a,b)
    }

    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}

