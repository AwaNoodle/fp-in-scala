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

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case MySome(_) => this
  }

  def filter(f: A => Boolean): MyOption[A] = {
    this.flatMap(x => if(f(x) == true) this else MyNone)
  }
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]
