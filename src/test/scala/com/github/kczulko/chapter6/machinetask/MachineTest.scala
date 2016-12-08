package com.kczulko.chapter6.machinetask

import com.kczulko.chapter6.machinetask.Machine.simulateMachine
import org.scalatest.{Matchers, FlatSpec}

class MachineTest extends FlatSpec with Matchers {

  "simulateMachine" should "proceed to unlock if there is a coin inserted into locked machine" in {
    val machine = simulateMachine(List(Coin())).run(Machine(true, 5, 5))._2

    machine.locked should be { false }
  }

  it should "dispense the candy and become locked when knob was turned on an unlocked machine" in {
    val machine = simulateMachine(List(Turn())).run(Machine(false, 5, 5))._2

    machine.locked shouldEqual true
    machine.candies shouldEqual 4
    machine.coins shouldEqual 5
  }

  it should "not change machine state when there are no coins inside" in {
    val machine = simulateMachine(List(Coin())).run(Machine(false, 0, 5))._2

    machine.locked shouldBe false
    machine.coins shouldEqual 1
    machine.candies shouldEqual 5
  }

  it should "does nothing when turning a knob on a locked machine" in {
    val machine = simulateMachine(List(Turn(), Turn(), Turn())).run(Machine(true, 5, 5))._2

    machine.locked shouldBe true
    machine.coins shouldEqual 5
    machine.candies shouldEqual 5
  }

  it should "increase amount of coins while inserting on any state" in {
    simulateMachine(List(Coin(), Coin(), Coin())).run(Machine(true, 5, 5)) shouldEqual {
      ((8, 5), Machine(false, 8, 5))
    }

    simulateMachine(List(Coin(), Coin(), Coin())).run(Machine(false, 5, 5)) shouldEqual {
      ((8, 5), Machine(false, 8, 5))
    }
  }

  it should "return expected amount of coins and candies after moving through variety of inputs" in {
    val machine = simulateMachine(List(Turn(), Coin(), Coin(), Turn(), Turn(), Coin(), Turn())).run(Machine(true, 8, 8))._2

    machine.locked shouldBe true
    machine.coins shouldEqual 11
    machine.candies shouldEqual 6
  }

}
