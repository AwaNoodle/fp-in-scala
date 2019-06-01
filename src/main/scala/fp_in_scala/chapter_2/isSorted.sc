def isSorted[A](as: Seq[A], f: (A,A) => Boolean): Boolean = {
  if(as.length <= 1) true
  else {
    if(f(as.head, as.tail.head)) isSorted(as.tail, f) else false
  }
}

def comparitor(lhs: Int, rhs: Int): Boolean = { lhs <= rhs }
println(s"A empty seq is sorted? ${isSorted(Seq[Int](), comparitor)}")
println(s"A seq of 1 is sorted? ${isSorted(Seq[Int](1), comparitor)}")
println(s"An ordered seq is sorted? ${isSorted(Seq[Int](1,2,3), comparitor)}")
println(s"An unordered seq is sorted? ${isSorted(Seq[Int](1,3,2), comparitor)}")
