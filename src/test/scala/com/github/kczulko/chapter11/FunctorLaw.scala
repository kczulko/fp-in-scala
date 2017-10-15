package com.github.kczulko.chapter11

import org.scalatest.{FlatSpec, Matchers}

trait FunctorLaw extends FlatSpec with Matchers {
  def functorOf[A, F[_]](functor: Functor[F])(suts: F[A]*): Unit = {
    it should "preserve the structure when mapped with identity function" in {
      suts foreach { sut =>
        functor.map(sut)(identity) shouldEqual sut
      }
    }
  }

}
