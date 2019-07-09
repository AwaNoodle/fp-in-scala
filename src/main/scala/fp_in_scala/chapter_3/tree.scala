package fp_in_scala.chapter_3

object tree {
  sealed trait Tree[+A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // 3.25
  def size[A](a: Tree[A]): Int = a match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  // 3.26
  def maximum(a: Tree[Int]): Int = {
    def internalFindMax(a: Tree[Int], currentMax: Int = Int.MinValue): Int = a match {
      case Leaf(x) => x max currentMax
      case Branch(l, r) => internalFindMax(l, currentMax) max internalFindMax(r, currentMax) 
    }

    internalFindMax(a)
  }

  // 3.27
  def depth[A](a: Tree[A]): Int = a match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
  }

  // 3.28
  def map[A, B](a: Tree[A])(f: A => B): Tree[B] = a match {
    case Leaf(x) => Leaf[B](f(x))
    case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
  }

  // f is how to convert the value in a lead
  // g is how to combine the results together
  def fold[A,B](a: Tree[A])(f: A => B)(g: (B,B) => B): B = a match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
