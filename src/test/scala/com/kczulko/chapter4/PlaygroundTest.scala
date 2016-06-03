package com.kczulko.chapter4

import com.kczulko.chapter4.Playground.{sequence, variance}
import org.scalatest.{FlatSpec, Matchers}

class PlaygroundTest extends FlatSpec with Matchers {

  "variance" should "return 0.25 for sequence of 1 and 2" in {
    variance(Seq(1,2)) shouldEqual Some(0.25)
  }

  it should "None for empty collection" in {
    variance(Seq()) shouldEqual None
  }

  "sequence" should "return Some[List] when all elements are defined" in {
    sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1,2,3))
  }

  it should "return None when any of elements is None" in {
    sequence(List(Some(1), Some(2), None)) shouldEqual None
  }
}
