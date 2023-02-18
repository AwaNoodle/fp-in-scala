package fp_in_scala.chapter_6

trait RNG {
  def nextInt: (Int, RNG)
}