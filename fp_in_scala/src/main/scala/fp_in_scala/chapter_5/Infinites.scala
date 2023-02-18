package fp_in_scala.chapter_5

object Infinites {
  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val f_h = () => a
    lazy val f_t = () => constant(a)
    Cons(f_h, f_t)
  }

  // 5.9
  def from(n: Int): Stream[Int] = {
    lazy val f_h = () => n
    lazy val f_t = () => from(n + 1)
    Cons(f_h, f_t)
  }

  // 5.10
  def fibs(): Stream[Int] = {
    def internalFibs(curr: Int, next: Int): Stream[Int] = {
      lazy val f_h = () => curr
      lazy val f_t = () => internalFibs(next, curr + next)
      Cons(f_h, f_t)
    }

    internalFibs(0,1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty[A]
    case Some((value, tail)) => Cons(() => value, () => unfold(tail)(f))
  }
}
