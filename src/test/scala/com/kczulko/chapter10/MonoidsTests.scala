package com.kczulko.chapter10

import com.kczulko.chapter10.Monoids._
import org.scalatest.{FlatSpec, Matchers}

class MonoidsTests extends FlatSpec with MonoidLaws with Matchers {
  "Integer addition" should behave like monoidOf(intAddition)(1,2,3)
  "Integer multiplication" should behave like monoidOf(intMultiplication)(1,2,3)
  "Boolean or" should behave like monoidOf(booleanOr)(true,false,true)
  "Boolean and" should behave like monoidOf(booleanAnd)(true,false,true)
  "Option monoid" should behave like monoidOf(optionMonoid: Monoid[Option[Boolean]])(Some(true), None, Some(true))
//  "Endo monoid" should behave like monoidOf(endoMonoid: Monoid[Int => Int])(v=>v+2, t=>t-1, u=>u*2)

  "foldMap" should "return expected result when folding whole list" in {
    foldMap(List("1", "2", "3"), intAddition)(_.toInt) shouldEqual 6
  }

  "foldRight with foldMap impl" should "behave like 'normal' foldRight" in {
    foldRight(List("1", "2", "3"), "")(_ + _) shouldEqual "123"
  }
//
//  "isOrdered" should "work as expected" in {
//    isOrdered(IndexedSeq(1,2,3,4)) shouldEqual true
//    isOrdered(IndexedSeq(1,2,4,3)) shouldEqual false
//    isOrdered(IndexedSeq(1,2)) shouldEqual true
//    isOrdered(IndexedSeq(2,1)) shouldEqual false
//    isOrdered(IndexedSeq(2)) shouldEqual true
//    isOrdered(IndexedSeq()) shouldEqual true
//  }

  "wc" should "properly count words" in {
    countWords("this is sparta!") shouldEqual 3
  }

  "productMonoid" should behave like
    monoidOf(
      productMonoid(optionMonoid: Monoid[Option[Boolean]], intAddition)
    )((Some(true), 2), (Some(false), 0), (None, 3))
}
