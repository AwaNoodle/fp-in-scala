package fp_in_scala.chapter_4

// 4.1
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }
  
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

  //def getOrElse[B >: A](default: => B): B
  //def orElse[B >: A](ob: => Option[B]): Option[B]
  //def filter(f: A => Boolean): Option[A]
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]
