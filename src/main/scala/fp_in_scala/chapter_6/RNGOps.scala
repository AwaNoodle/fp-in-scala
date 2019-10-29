package fp_in_scala.chapter_6

object RNGOps {
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
}