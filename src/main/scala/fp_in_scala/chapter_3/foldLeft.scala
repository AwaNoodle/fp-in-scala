package fp_in_scala.chapter_3

object foldLeft {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}
