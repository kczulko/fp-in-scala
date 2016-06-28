package com.kczulko.chapter6.machinetask

import com.kczulko.chapter6.State

case class Machine(locked: Boolean, coins: Int, candies: Int)

object Machine {

  private def toStateTransition(input: Input): State[Machine, Unit] = State(
    m => {
      (input, m.candies, m.locked) match {
        case (i: Coin, c, true) if c > 0 => ((), Machine(false, m.coins + 1, m.candies))
        case (i: Turn, c, false) if c > 0 => ((), Machine(true, m.coins, m.candies - 1))
        case (i: Coin, c, l) => ((), Machine(l, m.coins + 1, m.candies))
        case _ => ((), m)
      }
    }
  )

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldLeft(State[Machine, (Unit)](m => ((), m))){
      (b, input) => State(m => toStateTransition(input).run(b.run(m)._2))
    }.flatMap((Unit) => State(m => ((m.coins, m.candies), m)))
  }
}