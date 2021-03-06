package com.github.kczulko.chapter11

import org.scalatest.{FlatSpec, Matchers}

trait MonadLaws { this: FlatSpec with Matchers =>

  def monadOf[A, B, C, T[U] <: { def flatMap[V](fun: U => T[V]): T[V] }]
  (m: Monad[T], suts: T[A]*)
  (f: A => T[B])
  (g: B => T[C]): Unit = {

    it should "satisfy monad's associativity law" in {
      suts foreach { sut =>
        m.flatMap(sut)(f).flatMap(g) shouldEqual m.flatMap(sut)(a => f(a).flatMap(g))
      }
    }

    it should "satisfy identity function composition law" in {
      val f1 = m.compose[A,A,B](m.unit(_), f)
      val f2 = m.compose[A,B,B](f, m.unit(_))

      suts foreach { sut =>
        m.flatMap(sut)(f1) shouldEqual m.flatMap(sut)(f2)
      }
    }
  }
}
