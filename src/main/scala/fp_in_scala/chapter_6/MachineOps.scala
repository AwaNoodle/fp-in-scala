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
  
  type Candy = Int 
  val turnHandle : State[Machine, Option[Candy]] = State { 
    case Machine(false, candies, coins) if candies > 0 => (Some(1), Machine(true, candies - 1, coins))
    case current => (None, current)
  }

  val insertCoin: State[Machine, Unit] = State {
    case Machine(true, candies, coins) if candies > 0 => ((), Machine(false, candies, coins + 1))
    case current => ((), current)
  }

  def processAllInput(inputs: List[Input]): State[Machine, Unit] = 
    inputs.foldLeft(State.unit[Machine]){
      case (currentState, Coin) => currentState.flatMap(_ => insertCoin)
      case (currentState, Turn) => currentState.flatMap(_ => turnHandle).map(_ => ()) 
    }

  def simulateMachine(inputs: List[Input]): State[Machine, MachineStatus] = 
    for {
      _ <- processAllInput(inputs)
      state <- State.get[Machine]
    } yield {
      MachineStatus(state.candies, state.coins) 
    }
}
