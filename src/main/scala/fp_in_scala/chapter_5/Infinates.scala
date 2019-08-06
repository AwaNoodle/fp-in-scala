package fp_in_scala.chapter_5

object Infinates {
  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val f_h = () => a
    lazy val f_t = () => constant(a)
    Cons(f_h, f_t)
  }
}
