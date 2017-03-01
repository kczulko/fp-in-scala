package com.github.kczulko.chapter15

import com.github.kczulko.chapter15.Process.liftOne
import org.scalatest.{FlatSpec, Matchers}

class ProcessTest extends FlatSpec with Matchers {
  "liftOne" should "modify first element of the given stream" in {
    liftOne((i: Int) => i + 4)(Stream(3,4,5,6)).toList shouldEqual List(7)
  }

  "repeat" should "apply given function to all stream elements" in {
    liftOne[Int,Int](_ + 4).repeat(Stream(3,4,5,6)).toList shouldEqual List(7,8,9,10)
  }
}
