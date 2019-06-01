def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
 as match {
  case Nil => z
  case x :: xs => f(x, foldRight(xs, z)(f))
}

def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

val testList = List(1,2,3)
val result = foldRight(testList, Nil:List[Int])(_ :: _)
println(s"Nil FoldRight test is ${result}")

def length[A](as: List[A]): Int = foldRight(testList, 0)((_,b) => b + 1)
println(s"List ${testList} is ${length(testList)} long")

// 3.12
def reverseList(lst: List[Int]) = foldRight(lst, List[Int]())((a,b) => a +: b)
println(s"Reversing ${testList} is ${reverseList(testList)}")

// 3.14
def append[A](lst: List[A], item: A) = foldRight(lst, List(item))((a,b) => a +: b)
println(s"Appending 99 to ${testList} is ${append(testList, 99)}")
