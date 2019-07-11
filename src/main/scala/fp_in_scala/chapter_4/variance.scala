package fp_in_scala.chapter_4

// 4.2 - Implement the variance function in terms of flatMap.
object variance {
  def mean(xs: Seq[Double]) = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(x => mean(xs.map(y => math.pow(y - x, 2))))
}
