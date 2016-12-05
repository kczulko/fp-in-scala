package com.kczulko.chapter10

import com.kczulko.chapter10.Monoids._
import org.scalatest.{FlatSpec, Matchers}

class MonoidsTests extends FlatSpec with MonoidLaws with Matchers {
  "Integer addition" should behave like monoidOf(intAddition, (1,2,3))
  "Integer multiplication" should behave like monoidOf(intMultiplication, (1,2,3))
  "Boolean or" should behave like monoidOf(booleanOr, (true, false, true))
  "Boolean and" should behave like monoidOf(booleanAnd, (true, false, true))
}
