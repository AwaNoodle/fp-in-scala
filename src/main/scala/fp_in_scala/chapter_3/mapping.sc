@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case x :: xs => foldLeft(xs, f(z, x))(f)
}

// 3.18
def map[A,B](lst: List[A])(f: A => B): List[B] = {
  foldLeft(lst, List[B]())((b,a) => b :+ f(a))
}

// 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = {
  foldLeft(as, List[A]())((b,a) => if(f(a))  b :+ a else b)
}

// 3.20
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
  foldLeft(as, List[B]())((b,a) => b ::: f(a))
}

// 3.21
def filter_2[A](as: List[A])(f: A => Boolean): List[A] = {
  flatMap(as)(a => if(f(a)) List(a) else List[A]())
}

val intList = List(1,2,3,4)
val doubleList = List[Double](0.0, 0.1, 0.2, 0.3)

// 3.16
def addOne(a: Int): Int = a + 1
println(s"Mapping ${intList} by adding 1 is ${map(intList)(addOne)}")

// 3.17
def toString(a: Double): String = a.toString
println(s"Mapping ${doubleList} into strings is ${map(doubleList)(toString)}")

// 3.19
def isEven(a: Int) = a % 2 == 0
println(s"Filtering ${intList} for odd numbers is ${filter(intList)(isEven)}")

// 3.20
println(s"Flatmapping ${intList} is ${flatMap(intList)(i => List(i,i))}")

// 3.21
println(s"Filtering odd numbers in ${intList} via flatmap is ${filter_2(intList)(isEven)}")
