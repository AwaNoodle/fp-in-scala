package fp_in_scala.chapter_4

object lifting {
  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for {
      valA <- a
      valB <- b
    } yield f(valA, valB)

    //4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      // If h is None, then the flatMap will return None, else you keep building the list
      case h :: t => h.flatMap(valHead => sequence(t).map(tailList => valHead :: tailList))
      case Nil => Some(Nil)
    }

    //4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case h :: t => f(h).flatMap(valHead => traverse(t)(f).map(tailList => valHead :: tailList))
      case Nil => Some(Nil)
    }
}
