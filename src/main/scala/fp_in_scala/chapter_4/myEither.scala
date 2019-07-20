package fp_in_scala.chapter_4

// 4.6
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(x) => MyLeft[E, B](x)
    case MyRight(x) => MyRight[E, B](f(x))
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(x) => MyLeft[E, B](x)
    case MyRight(x) => f(x)
  }

  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(x) => MyRight[EE, B](x)
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    for {
      rightThis <- this
      rightB <- b
    } yield f(rightThis, rightB)
  }
}

final case class MyRight[+E, +A](right: A) extends MyEither[E, A]
final case class MyLeft[+E, +A](left: E) extends MyEither[E, A]
