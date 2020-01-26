package fp_in_scala

package object chapter_6 {
  type Candy           = Int
  type MachineState[A] = State[Machine, A]

  def processAllInput[Context[_]](
    inputs: List[Input]
  )(implicit monad: Monad[Context], machine: CandyMachine[Context]): Context[Unit] = {
    inputs.foldLeft(monad.unit) {
      case (currentContext, Coin) => currentContext.flatMap(_ => machine.insertCoin)
      case (currentContext, Turn) => currentContext.flatMap(_ => machine.turnHandle).map(_ => ())
    }
  }

  trait Monad[Context[_]] {
    def pure[A](a: A): Context[A]
    def flatMap[A, B](ca: Context[A])(f: A => Context[B]): Context[B]

    def unit: Context[Unit]                              = pure(())
    def map[A, B](ca: Context[A])(f: A => B): Context[B] = flatMap(ca)(x => pure(f(x)))
  }
  implicit class MonadOps[Context[_], A](c: Context[A])(implicit monad: Monad[Context]) {
    def flatMap[B](f: A => Context[B]): Context[B] = monad.flatMap(c)(f)
    def map[B](f: A => B): Context[B]              = monad.map(c)(f)
  }

}
