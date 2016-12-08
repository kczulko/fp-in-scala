package com.kczulko.chapter10

import com.kczulko.chapter10.Monoids._
import org.scalatest.{FlatSpec, Matchers}

class MonoidsTests extends FlatSpec with MonoidLaws with Matchers {
  "Integer addition" should behave like monoidOf(intAddition)(1,2,3)
  "Integer multiplication" should behave like monoidOf(intMultiplication)(1,2,3)
  "Boolean or" should behave like monoidOf(booleanOr)(true,false,true)
  "Boolean and" should behave like monoidOf(booleanAnd)(true,false,true)
  "Option monoid" should behave like monoidOf(optionMonoid: Monoid[Option[Boolean]])(Some(true), None, Some(true))

  "foldMap" should "return expected result when folding whole list" in {
    foldMap(List("1", "2", "3"), intAddition)(_.toInt) shouldEqual 6
  }

  "foldRight with foldMap impl" should "behave like 'normal' foldRight" in {
    foldRight(List("1", "2", "3"), "")(_ + _) shouldEqual "123"
  }

  "wc" should "properly count words" in {
    countWords("this is sparta!") shouldEqual 3
  }

  "productMonoid" should behave like monoidOf {
    productMonoid(optionMonoid: Monoid[Option[Boolean]], intAddition)
  }((Some(true), 2), (Some(false), 0), (None, 3))

  "bag" should "produce map with a number of each occurence" in {
    bag(Vector("a", "rose", "is", "a", "rose")) should contain theSameElementsAs {
      Map("a" -> 2, "rose" -> 2, "is" -> 1)
    }
  }
}
