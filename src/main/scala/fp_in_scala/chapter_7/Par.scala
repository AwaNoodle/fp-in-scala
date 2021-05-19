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

  // Ex 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit[B](f(a))

  // Ex 7.5
//  def sequence[A](ps: List[Par[A]]): Par[List[A]] = (es: ExecutorService) => {
//    es.submit(new Callable[List[A]] {
//      def call = ps.foldLeft(List[A]()) { (resList, pA) => resList :+ pA(es).get }
//    })
//  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]())) { (pA, resList) => map2(pA, resList) { _ :: _ } }
  }

  // Book listing
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Ex 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // convert the List[A] to a List[Par[List[A]]]
    // Using List A so we can flatten out filtered value
    val filtered = as.map(asyncF((a: A) => if(f(a)) List(a) else List() ))

    // Convert to Par[List[List[A]]] -- Ready to flatten
    val sequenced = sequence(filtered)

    // Need to convert the List[List[A]] to a List[A]
    // So we need a map
    // Is UnitFuture a good idea here though?
    def map[B,C](l: Par[B])(f: B => C): Par[C] = {
      (es: ExecutorService) => {
        var b = l(es)
        UnitFuture(f(b.get))
      }
    }

    map(sequenced)(_.flatten)
  }
}

