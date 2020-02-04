package fp_in_scala.chapter_7

class Par[A] {

}

object Par {
  // 7.1
  def Map2[A, B,C](l: Par[A], r: Par[B])(f: (A,B) => C) : Par[C] = ???
}

