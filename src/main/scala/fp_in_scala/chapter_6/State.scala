package fp_in_scala.chapter_6

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap { a: A => State(s => (f(a), s)) }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State { s =>
      val (aRes, aNext) = run(s)
      g(aRes).run(s)
    }
  }
}

object State {
  def unit[S, A](a: A) = State((s: S) => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(State.unit[S, List[A]](Nil)) { (currState, next) =>
      map2(next, currState)((lstA, b) => b +: lstA)
    }
  }

  def map2[S,A,B,C](sa: State[S, A], sb: State[S,B])(f: (A, B) => C): State[S,C] = {
    sa.flatMap { a =>
      sb.map { b => 
        f(a,b)
      } 
    }
  }
}
