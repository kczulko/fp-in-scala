package com.github.kczulko.chapter11

import org.scalatest.{FlatSpec, Matchers}

class MonadsTest extends FlatSpec with Matchers with MonadLaws {
  "Id monad" should behave like monadOf(IdMonad, Id(5)){
    i => Id(i.toString)
  }{
    s => Id(s.hashCode.toFloat)
  }

  "Option monad" should behave like monadOf(Monads.optionMonad[Int], Some(2)){
    (_: Int) => None
  }{
    (_: Int) => Some("some")
  }
}
