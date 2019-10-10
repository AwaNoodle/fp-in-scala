package fp_in_scala.chapter_5

import org.scalatest.{FlatSpec, Matchers}

class Infinites_spec extends FlatSpec with Matchers {
  import Infinites._
  
  // 5.8
  "constant" should "produce an infinite stream of type A" in {
    val constant_a = constant("a")
    constant_a.take(5).toList shouldBe List("a","a","a","a","a")
    constant_a.take(20).toList shouldBe (1 to 20).map(_ => "a").toList

    val constant_ten = constant(10)
    constant_ten.take(107).toList shouldBe (1 to 107).map(_ => 10).toList
  }

  // 5.9
  "from" should "produce a stream of incrementing ints" in {
    from(5).take(5).toList shouldBe List(5,6,7,8,9)
    from(1).take(1000).toList shouldBe (1 to 1000).toList
  }

  // 5.10
  "fibs" should "produce a stream of Fibbonaci numbers" in {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  // 5.11
  "unfold" should "produce a stream from the supplied function" in { 
    def inc(currState: Int): Option[(Int, Int)] = {
      Some((currState, currState + 1))
    }

    unfold(0)(inc).take(7).toList shouldBe (0 to 6).toList
  }

  it should "finish the stream when the func returns None" in {
    def inc(currState: Int): Option[(Int, Int)] = {
      if(currState < 5) {
        Some((currState, currState + 1))
      } else {
        None
      }
    }

    unfold(0)(inc).take(7).toList shouldBe (0 to 4).toList
  }

  // 5.12
  it should "allow an implementation of fibs" in {
    // Pair is previous, current
    def fibs(s: (Int, Int)): Option[(Int, (Int, Int))] = {    
      val current = if(s._2 == 0) 1 else s._2
      Some((s._2, (s._2, s._1 + current)))
    }

    unfold((0,0))(fibs)
      .take(7)
      .toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "allow an implementation of from" in {
    def from(current: Int): Option[(Int,Int)] = Some(current -> (current + 1))

    unfold(5)(from).take(5).toList shouldBe List(5,6,7,8,9)
    unfold(1)(from).take(1000).toList shouldBe (1 to 1000).toList
  }

  it should "allow an implementation of constant" in {
    def constant[T](value: T): Option[(T, T)] = Some(value -> value)

    unfold("a")(constant).take(5).toList shouldBe List("a","a","a","a","a")
    unfold(10)(constant).take(107).toList shouldBe (1 to 107).map(_ => 10).toList
  }

  it should "allow an implementation of ones" in {
    def ones(value: Int): Option[(Int, Int)] = Some(value -> value)

    unfold(1)(ones).take(10) shouldBe (1 to 10).map(_ => 1)
  }

  // 5.13 
  it should "allow an implementation of map" in {
    def map[A, B](stream : Stream[A])(f : A => B) : Stream[B] = 
      unfold[B, Stream[A]](stream) { 
        case Empty => None
        case Cons(head, tail) => Some(f(head()) -> tail())
      }

    val testStream = unfold(1)(x => Some[(Int, Int)](x -> x))
    map(testStream)(x => x +1).take(5) shouldBe List(2,2,2,2,2)
  }

  it should "allow an implementation of take" in {
    def take[A](stream: Stream[A])(amt: Int): List[A] = {
      val wrapper = (stream -> amt)
      unfold(wrapper) {
        case (Empty, _) => None
        case (_, acc) if acc == 0 => None
        case (Cons(head, tail), acc) => Some((head(), (tail() -> (acc - 1))))
      }.toList
    }

    val testStream = constant(5)
    take(testStream)(3) shouldBe List(5,5,5)
  }

  it should "allow an implementation of takeWhile" in {
    def take[A](stream: Stream[A])(f: A => Boolean): Stream[A] = {
      unfold(stream) {
        case Cons(head, tail) if !f(head()) => None
        case Cons(head, tail) => Some((head(), tail()))
        case _ => None
      }
    }

    val testStream = from(1)
    take(testStream)(x => x < 4).toList shouldBe List(1,2,3)
  }

  it should "allow an implementation of zipWith" in {
    def zipWith[A,B](a: Stream[A], b: Stream[B]): Stream[(A,B)] = {
      unfold((a,b)) {
        case (Empty, _) => None
        case (_, Empty) => None
        case (Cons(headA, tailA), Cons(headB, tailB)) => Some(((headA(), headB()), (tailA(), tailB())))
      }
    }

    val testA = Stream(1,2,3,4,5) 
    val testB = Stream("a","b","c","d")
    zipWith(testA, testB).take(5) shouldBe List((1,"a"),(2, "b"),(3, "c"),(4, "d"))
  }

  it should "allow an implementation of zipAll" in {
    def zipAll[A,B](a: Stream[A], b: Stream[B]): Stream[(Option[A],Option[B])] = {
      unfold((a,b)) {
        case (Empty, Cons(headB, tailB)) => Some(((None, Some(headB())), (Empty, tailB())))
        case (Cons(headA, tailA), Empty) => Some(((Some(headA()), None)), (tailA(), Empty))
        case (Cons(headA, tailA), Cons(headB, tailB)) => Some(((Some(headA()), Some(headB())), (tailA(), tailB())))
        case _ => None
      }
    }

    val testA = Stream(1,2,3,4,5) 
    val testB = Stream("a","b","c","d")
    zipAll(testA, testB).take(10) shouldBe List((Some(1), Some("a")),
                                                (Some(2), Some("b")),
                                                (Some(3), Some("c")),
                                                (Some(4), Some("d")),
                                                (Some(5), None))
  }
}
