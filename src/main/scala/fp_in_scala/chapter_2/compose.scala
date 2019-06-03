package fp_in_scala.chapter_2

object compose {
 def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
 }
}
