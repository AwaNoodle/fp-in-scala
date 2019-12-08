package fp_in_scala.chapter_6

import State._

// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insertCoin(): Machine = if(candies > 0) {
    Machine(false, candies, coins + 1)
  } else {
    this
  }

  def turnHandle(): Machine = if(!locked && candies > 0) {
    Machine(locked, candies - 1, coins)
  } else {
    this
  }
}

object MachineOps {
  final case class MachineStatus(candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, MachineStatus] = {
    State{s =>
      inputs.foldLeft((MachineStatus(s.candies, s.coins), s)) { (acc, input) => input match {
          case Coin => {
            val newState = acc._2.insertCoin
            (MachineStatus(newState.candies, newState.coins), newState)
          }
          case Turn => {
            val newState = acc._2.turnHandle
            (MachineStatus(newState.candies, newState.coins), newState)
          }
        }
      }
    }
  }
}
