package fp_in_scala.chapter_2

object isSorted {
  def isSorted[A](as: Seq[A], f: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else {
      if (f(as.head, as.tail.head)) isSorted(as.tail, f) else false
    }
  }

  def comparitor(lhs: Int, rhs: Int): Boolean = {
    lhs <= rhs
  }
}
