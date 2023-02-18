package fp_in_scala.chapter_3

object zip {
  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) | (_, Nil)  => Nil
    case (ah :: at, bh :: bt) => f(ah, bh) +: zipWith(at, bt)(f)
  }
}
