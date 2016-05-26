package com.kczulko.chapter2

import com.kczulko.chapter2.Exercise_2_2.isSorted
import org.scalatest.{FlatSpec, Matchers}

class Exercise_2_2_Tests extends FlatSpec with Matchers {
  val ascending = (first: Int, second: Int) => first < second
  val ascendingOrEqual = (first: Int, second: Int) => first <= second
  val descendingOrEqual = (first: Int, second: Int) => first >= second

  "isSorted" should "return true when array is empty or has single element" in {

    isSorted(Array(), ascending) shouldEqual true
    isSorted(Array(5), ascending) shouldEqual true
  }

  it should "return true for ascendingOrder comparator" in {
    isSorted(Array(0, 2, 5, 7, 9), ascending) shouldEqual true
  }

  it should "return false for ascendingOrder comparator applied with descending Array" in {
    isSorted(Array(9, 5, 3, 7), ascending) shouldEqual false
  }

  it should "return true for descendingOrEqual comparator" in {
    isSorted(Array(9, 5, 5, 4), descendingOrEqual) shouldEqual true
  }
}
