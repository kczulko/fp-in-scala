package com.kczulko.chapter10

import org.scalatest.{FlatSpec, Matchers}

trait MonoidLaws { this: FlatSpec with Matchers =>
  def monoidOf[A](monoid: Monoid[A], values: (A, A, A)) {
    it should "satisfy associativity law for op" in {
      val a1 = values._1
      val a2 = values._2
      val a3 = values._3

      monoid.op(monoid.op(a1, a2), a3) shouldEqual monoid.op(a1, monoid.op(a2, a3))
    }

    it should "satisfy left and right invariant law for op operation and zero arg" in {
      val a1 = values._1

      monoid.op(monoid.zero, a1) shouldEqual monoid.op(a1, monoid.zero)
    }
  }
}

