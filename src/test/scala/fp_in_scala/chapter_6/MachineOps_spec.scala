package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class MachineOps_spec extends FlatSpec with Matchers {
  import MachineOps._

  val fullLockedMachine = Machine(true, 10, 0)

  "A machine with candy" should "unlock when a coin is added" in {
    val inputs = List(Coin)
    simulateMachine(inputs).run(fullLockedMachine) shouldBe (MachineStatus(10,0), Machine(true, 10, 0))
  }
}
