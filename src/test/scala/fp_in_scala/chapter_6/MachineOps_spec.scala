package fp_in_scala.chapter_6

import org.scalatest.{FlatSpec, Matchers}

class MachineOps_spec extends FlatSpec with Matchers {
  import MachineOps._

  val fullLockedMachine = Machine(true, 10, 0)
  val emptyLockedMachine = Machine(true, 0, 0)
  val emptyUnlockedMachine = Machine(false, 0, 0)

  "A machine with candy" should "unlock when a coin is added" in {
    val inputs = List(Coin)
    simulateMachine(inputs).run(fullLockedMachine) shouldBe (MachineStatus(10,1), Machine(false, 10, 1))
  }

  it should "unlock and return candy when a coin is inserted and the handle is turned" in {
    val inputs = List(Coin, Turn)
    simulateMachine(inputs).run(fullLockedMachine) shouldBe (MachineStatus(9,1),Machine(false, 9, 1))
  }

  "A machine without candy" should "stay locked if a coin is added and return coin" in {
    val inputs = List(Coin)
    simulateMachine(inputs).run(emptyLockedMachine) shouldBe (MachineStatus(0,0), Machine(true, 0, 0))
  }

  it should "stay unlocked if a coin is added and return the coin" in {
    val inputs = List(Coin)
    simulateMachine(inputs).run(emptyUnlockedMachine) shouldBe (MachineStatus(0,0), Machine(false, 0, 0))
  }
}
