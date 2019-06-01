def tail[A](seq: Seq[A]): Seq[A] = seq match {
  case Nil => Nil
  case _ :: xs => xs
}

def setHead[A](newHead: A, seq: Seq[A]): Seq[A] = newHead +: seq.tail

def drop[A](l: List[A], n: Int): List[A] = {
  if (l == Nil) Nil
  else if(n <= 0) l
  else drop(l.tail, n - 1)
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case x :: xs if f(x) => dropWhile(xs, f)
  case x :: xs => x +: dropWhile(xs, f)
}

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case _ :: Nil => Nil
  case x :: xs => x +: init(xs)
}

val test = List(1,2,3,4,5,6)
println(s"The tail of ${test} is ${tail(test)}")
println(s"The tail of ${Nil} is ${tail(Nil)}")

val headTest = List("b", "c", "d")
println(s"Adding 'a' to ${headTest} is ${setHead("a", headTest)}")

println(s"Droping 2 from ${test} is ${drop(test, 2)}")
println(s"Droping 4 from ${test} is ${drop(test, 4)}")

def isDivisableByTwo(x: Int): Boolean = (x % 2) == 0
println(s"DropWhile mod 2 on ${test} is ${dropWhile(test, isDivisableByTwo)}")

println(s"Init on ${test} returns ${init(test)}")
