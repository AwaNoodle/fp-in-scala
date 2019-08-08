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
}
