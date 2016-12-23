package com.github.kczulko.chapter11

import com.github.kczulko.chapter11.Monads.{eitherMonad, idMonad, optionMonad}
import com.kczulko.chapter4.MyEither
import org.scalatest.{FlatSpec, Matchers}

class MonadsTest extends FlatSpec with Matchers with MonadLaws {
  "Id monad" should behave like monadOf(idMonad, Id(5)){
    i => Id(i.toString)
  }{
    s => Id(s.hashCode.toFloat)
  }

  "Option monad" should behave like monadOf(optionMonad, Some(2)){
    (_: Int) => None
  }{
    (_: Int) => Some("some")
  }

  MyEither
}
