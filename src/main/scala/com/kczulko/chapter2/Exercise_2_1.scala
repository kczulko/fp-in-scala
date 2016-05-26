package com.kczulko.chapter2

import scala.annotation.tailrec

object Exercise_2_1 {
  def fibonacci(n: Int) = {
    require(n >= 0, "This function does not accept negative values.")

    n match {
      case 0 | 1 => n
      case _ => {
        @tailrec
        def loop(counter: Int, first: Int, second: Int): Int = {
          if (counter == n) {
            first + second
          } else {
            loop(counter + 1, second, first + second)
          }
        }
        loop(2, 0, 1)
      }
    }
  }
}
