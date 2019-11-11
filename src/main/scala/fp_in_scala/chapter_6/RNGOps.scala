package fp_in_scala.chapter_6

object RNGOps {
  // From text
  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

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

  // 6.2 (replaced by 6.8)
  // def double(rng: RNG): (Double, RNG) = {
  //   val nonNeg = nonNegativeInt(rng)
  //   (nonNeg._1 / (Int.MaxValue.toDouble + 0.1), nonNeg._2)
  // }

  // 6.5
  val double: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 0.1))
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, iNext) = rng.nextInt
    val (d, dNext) = double(iNext)
    ((i, d), dNext)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), next) = intDouble(rng)
    ((d,i), next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (a, aNext) = double(rng)
    val (b, bNext) = double(aNext)
    val (c, cNext) = double(bNext)

    ((a,b,c), cNext)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft((List[Int](), rng)) { (state, ignore) =>
      val (i, next) = state._2.nextInt
      (state._1 :+ i, next)
    }
  }

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (resA, rngA) = ra(rng)
      val (resB, rngB) = rb(rngA)
      (f(resA, resB), rngB) 
    }
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { 
    rng => {
      fs.foldLeft((List[A](), rng)) { (state, next) => 
        val (nxtRes, nxtRng) = next(rng)
        (state._1 :+ nxtRes, nxtRng) 
      }
    }
  }
}