package fp_in_scala.chapter_6

object RNGOps {
  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    
    if(next._1 == Int.MinValue) {
      (Int.MaxValue, next._2)
    } else if(next._1 < 0) {
      (Math.abs(next._1), next._2)
    } else {
      next
    }
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val nonNeg = nonNegativeInt(rng)
    (nonNeg._1 / (Int.MaxValue.toDouble + 0.1), nonNeg._2)
  }
}