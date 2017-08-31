package com.github.kczulko.chapter8.myimpl

import com.kczulko.chapter6.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class SGenTest extends FlatSpec with Matchers {

  it should "return success while validating List.max function" in {

    val smallInt = Gen.choose(-10, 10)
    val maxProp = SGen.forAll(SGen.nonEmptyListOf(smallInt)) {
      list =>
        val max = list.max
        !list.exists(_ > max)
    }

    maxProp.run(100, 100, SimpleRNG(System.currentTimeMillis)) shouldEqual Passed
  }

  it should "return success while checking List.sorted property" in {

    def propOf(f: List[Int] => Boolean) = {
      val ints = Gen.choose(-100, 100)
      SGen.forAll(SGen.nonEmptyListOf(ints))(f)
    }

    val lastIsMax = propOf {
      list =>
        val sorted = list.sorted
        sorted.last == list.max
    }

    val firstIsMin = propOf {
      list =>
        val sorted = list.sorted
        sorted.head == list.min
    }

    val nextIsBiggerThanPrevious = propOf { list =>
      list.sorted match {
        case sorted @_ :: next :: tail =>
          (next :: tail).zip(sorted.init).forall({ case (bigger, lesser) => bigger >= lesser })
        case _ =>
          true
      }
    }

    (lastIsMax && firstIsMin && nextIsBiggerThanPrevious)
      .run(
        100,
        100,
        SimpleRNG(System.currentTimeMillis)
      ) shouldEqual Passed
  }

}
