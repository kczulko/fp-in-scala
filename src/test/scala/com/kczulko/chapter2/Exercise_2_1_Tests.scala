package com.kczulko.chapter2

import com.kczulko.chapter2.Exercise_2_1.fibonacci
import org.scalatest._

class Exercise_2_1_Tests extends FlatSpec with Matchers {
  "fibonacci" should "return valid values for the first two parameters" in {
    fibonacci(0) should be (0)
    fibonacci(1) should be (1)
  }

  it should "reject negative parameters" in {
    a [IllegalArgumentException] should be thrownBy {
      fibonacci(-2)
    }
  }

  it should "return expected result for expected input" in {
    val results = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377)
    for (i <- 0 to  results.length - 1)
      fibonacci(i) should be (results(i))
  }
}
