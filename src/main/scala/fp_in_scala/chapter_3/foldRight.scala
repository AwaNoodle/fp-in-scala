package fp_in_scala.chapter_3

object foldRight {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil     => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def sum(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  // 3.12
  def reverseList(lst: List[Int]) = foldRight(lst, List[Int]())((a, b) => b :+ a)

  // 3.14
  def append[A](lst: List[A], item: A) = foldRight(lst, List(item))((a, b) => a +: b)
}
