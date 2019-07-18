package fp_in_scala.chapter_4

object lifting {
  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for {
      valA <- a
      valB <- b
    } yield f(valA, valB)

    //4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      Some(for {
        optA <- a
        valA <- optA
      } yield valA)
    }
}
