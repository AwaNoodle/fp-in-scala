package fp_in_scala.chapter_6

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap { a: A => State(s => (f(a), s)) }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State { s =>
      val (aRes, sNext) = run(s)
      g(aRes).run(sNext)
    }
  }
}
  
object State {

  def pure[S, A](a: A) = State((s: S) => (a, s))
  def unit[S] : State[S, Unit] = pure(())

  def get[S] : State[S, S] = State((s: S) => (s,s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(State.pure[S, List[A]](Nil)) { (currState, next) =>
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
