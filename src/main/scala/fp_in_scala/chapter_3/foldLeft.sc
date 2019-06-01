@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case x :: xs => foldLeft(xs, f(z, x))(f)
}

val testIntList = List(1,2,3,4)

// 3.11
def sum(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
println(s"List ${testIntList} sums to ${sum(testIntList)}")

def product(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)
println(s"List ${testIntList} product is ${product(testIntList)}")

def length[A](as: List[A]): Int = foldLeft(testIntList, 0)((a,_) => a + 1)
println(s"List ${testIntList} is ${length(testIntList)} long")

// 3.12
def reverseList(lst: List[Int]) = foldLeft(lst, List[Int]())((b, a) => a +: b)
println(s"Reversing ${testIntList} is ${reverseList(testIntList)}")
