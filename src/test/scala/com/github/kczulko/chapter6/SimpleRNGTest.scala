package com.kczulko.chapter6

import org.scalatest.{Matchers, FlatSpec}

class SimpleRNGTest extends FlatSpec with Matchers {

  def createGenerators(count: Int): List[RNG] = {
    assert(count >= 1)

    (1 to count - 1).foldLeft(List(SimpleRNG(0)): List[RNG])(
      (b, i) => List(b.head.nextInt._2) ++ b
    )
  }

  "nonNegativeInt" should "return positive Integer numbers" in {
    (1 to 100).foldLeft(List(SimpleRNG(0)): List[RNG])(
      (b, i) => b.head.nonNegativeInt(b.head)._2 :: b
    ).map(rng => rng.nonNegativeInt(rng)._1).exists(_ < 0) shouldNot be (true)
  }

  "double" should "return double values between 0 and 1 (not including 1)" in {
    (1 to 100).foldLeft(List(SimpleRNG(0)): List[RNG])(
      (b, i) => List(b.head.double.run(b.head)._2) ++ b
    ).map(rng => rng.double.run(rng)._1).exists(p => p >= 1 || p < 0) shouldNot be (true)
  }

  "ints" should "return list of random integers" in {
    SimpleRNG(0).ints(2) should have length 2
    SimpleRNG(0).ints(2) shouldBe List(SimpleRNG(0).nextInt._2.nextInt._1, SimpleRNG(0).nextInt._1)
  }

  "sequence" should "return last Rand transformed with given function" in {
    val g = SimpleRNG(0)
    g.sequence(List(g.unit(1), g.unit(2), g.unit(3))).run(g)._1 shouldEqual List(1,2,3)
  }

  it should "as underlying implementation of ints2 generate the same results as ints" in {
    SimpleRNG(0).ints(2).reverse shouldBe SimpleRNG(0).ints2(2).run(SimpleRNG(0))._1
  }
}
