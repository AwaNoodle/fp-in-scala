package fp_in_scala.chapter_5

sealed trait Stream[+A] {
  // 5.1
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => List(h()) ::: t().toList
  }

  // 5.2
  def take(n: Int): List[A] = {
    if (n <= 0) Nil
    else {
      this match {
        case Empty      => Nil
        case Cons(h, t) => List(h()) ::: t().take(n - 1)
      }
    }
  }

  // 5.2
  def drop(n: Int): Stream[A] = {
    if (n <= 0) this
    else
      this match {
        case Empty      => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty                => Empty
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _                    => Empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => true
  }

  // 5.5
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def takeWhile_2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A]) { (a, funcB) =>
      if (p(a)) Stream.cons(a, funcB)
      else funcB
    }
}

case object Empty                                   extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
