package fp_in_scala.chapter_6

import State._
import scala.concurrent.Future

// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

trait CandyMachine[Context[_]] {

  def turnHandle: Context[Option[Candy]]
  def insertCoin: Context[Unit]

}

trait KVStore[F[_]] {
  def put(key: String, value: String): F[Unit]
  def get(key: String): F[Option[String]]
}

abstract class KVStoreSync extends KVStore[MachineOps.Id]

object MachineOps extends CandyMachine[MachineState] {
  type Id[A] = A
  final case class MachineStatus(candies: Int, coins: Int)

  val turnHandle: MachineState[Option[Candy]] = State {
    case Machine(false, candies, coins) if candies > 0 =>
      (Some(1), Machine(true, candies - 1, coins))
    case current => (None, current)
  }

  val insertCoin: MachineState[Unit] = State {
    case Machine(true, candies, coins) if candies > 0 => ((), Machine(false, candies, coins + 1))
    case current                                      => ((), current)
  }

}

object Program {
  import MachineOps._

  implicit val candyMachine: CandyMachine[MachineState] = MachineOps

  def simulateMachine(inputs: List[Input]): MachineState[MachineStatus] =
    for {
      _     <- processAllInput[MachineState](inputs)
      state <- State.get[Machine]
    } yield {
      MachineStatus(state.candies, state.coins)
    }
}
