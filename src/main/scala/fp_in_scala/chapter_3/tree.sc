sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

val simpleTree = Branch(Leaf(1), Leaf(2)) // 3 nodes, max 2 deep
val fullTree = Branch(simpleTree, Branch(simpleTree, Branch(Leaf(3), Leaf(4)))) // 11 nodes, max 4 deep

// 3.25
def countNodes[A](a: Tree[A]): Int = a match {
  case Leaf(_) => 1
  case Branch(l, r) => countNodes(l) + countNodes(r) + 1
}

println(s"A simpleTree has ${countNodes(simpleTree)} nodes")
println(s"A fullTree has ${countNodes(fullTree)} nodes")

// 3.26
def findMaxValue(a: Tree[Int]): Int = {
  def internalFindMax(a: Tree[Int], currentMax: Int = Int.MinValue): Int = a match {
    case Leaf(x) => x max currentMax
    case Branch(l, r) => internalFindMax(l, currentMax) max internalFindMax(r, currentMax) 
  }

  internalFindMax(a)
}

println(s"A simpleTree's largest leaf is ${findMaxValue(simpleTree)}")
println(s"A fullTree's largest leaf is ${findMaxValue(fullTree)}")

// 3.27
def depth[A](a: Tree[A]): Int = a match {
  case Leaf(_) => 1
  case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
}

println(s"A simpleTree's max depth is ${depth(simpleTree)}")
println(s"A fullTree's max depth is ${depth(fullTree)}")

// 3.28
def map[A, B](a: Tree[A])(f: A => B): Tree[B] = a match {
  case Leaf(x) => Leaf[B](f(x))
  case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
}

def addTwo(a: Int) = a + 2
println(s"A simpleTree's mapped adding 2 is ${map(simpleTree)(addTwo)}")
println(s"A fullTree's mapped adding 2 is ${map(fullTree)(addTwo)}")
