package fp_in_scala.chapter_6

import State._

// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object MachineOps {
  final case class MachineStatus(coins: Int, candies: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, MachineStatus] = ???
}
