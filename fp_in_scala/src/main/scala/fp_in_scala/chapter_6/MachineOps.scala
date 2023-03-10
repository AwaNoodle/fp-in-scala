package fp_in_scala.chapter_6

import State._
import scala.concurrent.Future

// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// Define what options a candy machine should have
// We're saying that a candy machine needs some kind of
// context. It could be a State, IO, something else
// So we would wrap the return of the thing we want in
// the context. i.e. inserting the coin could give us an
// IO[Unit] if we used a CandyMachine[IO]
// This could be F[_] with no difference - Context is the name
// Not a type
trait CandyMachine[Context[_]] {
  def turnHandle: Context[Option[Candy]]
  def insertCoin: Context[Unit]
}

// Example of KVStore taking in a generic context
trait KVStore[F[_]] {
  def put(key: String, value: String): F[Unit]
  def get(key: String): F[Option[String]]
}

abstract class KVStoreSync extends KVStore[MachineOps.Id]

// Implementation of our candy machine
// We solidify the context here by specifying
// the machine state which is a state monad of Machine and A
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

  // This is supplied to `processAllInput` as the second implicit param
  // The first implict param is from State where there is a definition of Monad
  // for State[S,A] which in our case is MachineState[A]
  implicit val candyMachine: CandyMachine[MachineState] = MachineOps

  def simulateMachine(inputs: List[Input]): MachineState[MachineStatus] =
    for {
      _     <- processAllInput[MachineState](inputs)
      state <- State.get[Machine]
    } yield {
      MachineStatus(state.candies, state.coins)
    }
}
