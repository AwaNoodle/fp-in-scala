package fp_in_scala.chapter_6

final case class State[S, +A](run: S => (A, S)) {

}

object State {
  def unit[S, A](a: A) = State((s: S) => (a, s))
}
