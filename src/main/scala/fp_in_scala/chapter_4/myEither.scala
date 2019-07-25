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

object MyEitherOps {
  // 4.7
  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
    case h :: t => h.flatMap(valHead => sequence(t).map(tailList => valHead :: tailList))
    case Nil => MyRight(Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match {
    case Nil => MyRight(Nil)
    case h :: t => f(h).flatMap(valHead => traverse(t)(f).map(tailList => valHead :: tailList))
  }
}

final case class MyRight[+E, +A](right: A) extends MyEither[E, A]
final case class MyLeft[+E, +A](left: E) extends MyEither[E, A]
