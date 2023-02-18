package fp_in_scala.chapter_3

object subsequence {
  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val taken = sup.take(sub.length)
    if (taken.length < sub.length) false
    else if (taken == sub) true
    else hasSubsequence(sup.tail, sub)
  }
}
