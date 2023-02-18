package fp_in_scala.chapter_3

object mapping {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  // 3.18
  def map[A, B](lst: List[A])(f: A => B): List[B] = {
    foldLeft(lst, List[B]())((b, a) => b :+ f(a))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldLeft(as, List[A]())((b, a) => if (f(a)) b :+ a else b)
  }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, List[B]())((b, a) => b ::: f(a))
  }

  // 3.21
  def filter_2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else List[A]())
  }
}
