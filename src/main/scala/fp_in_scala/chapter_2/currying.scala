package fp_in_scala.chapter_2

object currying {
  def currying[A, B, C](f: (A, B) => C): A => (B => C) = { a => b =>
    f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) =>
    f(a)(b)
  }
}
