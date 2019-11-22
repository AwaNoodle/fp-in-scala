package fp_in_scala.chapter_6

object RNGOps {
  // From text
  // case class State[S,+A](run: S => (A,S))
  
  type State[S,+A] = S => (A,S)
  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Orig from text, Replace 6.8
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a =>
      rng => (f(a), rng)
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

  // 6.2 (replaced by 6.5)
  // def double(rng: RNG): (Double, RNG) = {
  //   val nonNeg = nonNegativeInt(rng)
  //   (nonNeg._1 / (Int.MaxValue.toDouble + 0.1), nonNeg._2)
  // }

  // 6.5
  val double: Rand[Double] = {
    map(nonNegativeInt(_))(_ / (Int.MaxValue.toDouble + 0.1))
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

  // Orig 6.6 - Replace 6.8
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb) { b => 
        f(a,b)
      } 
    }
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft[Rand[List[A]]](rng => (List[A](), rng)) { (state, next) =>
      map2(next, state)(_ :: _)
    }
  }

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (aRes, aNext) = f(rng)
      g(aRes)(aNext)
    }
  }
}
