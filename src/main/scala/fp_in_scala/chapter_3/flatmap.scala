package fp_in_scala.chapter_3

object flatmap {

  def flatmap[T](lsts: List[List[T]]): List[T] = {
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }

    foldLeft(lsts, List[T]())((b, a) => b ::: a)
  }
}