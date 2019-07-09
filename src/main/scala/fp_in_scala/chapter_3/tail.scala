package fp_in_scala.chapter_3

object tail {
  // 3.2
  def tail[A](seq: Seq[A]): Seq[A] = seq match {
    case Nil => Nil
    case _ :: xs => xs
  }

  // 3.3
  def setHead[A](newHead: A, seq: Seq[A]): Seq[A] = newHead +: seq.tail

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (l == Nil) Nil
    else if(n <= 0) l
    else drop(l.tail, n - 1)
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs if f(x) => dropWhile(xs, f)
    case x :: xs => x +: dropWhile(xs, f)
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case x :: xs => x +: init(xs)
  }
}
