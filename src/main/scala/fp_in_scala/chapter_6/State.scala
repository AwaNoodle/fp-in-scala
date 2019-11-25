package fp_in_scala.chapter_6

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap { a: A =>
      State(s => (f(a), s))
    }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State { s =>
      val (aRes, aNext) = run(s)
      g(aRes).run(s)
    }
  }
}

object State {
  def unit[S, A](a: A) = State((s: S) => (a, s))
}
