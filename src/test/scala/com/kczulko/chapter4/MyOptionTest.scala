package com.kczulko.chapter4

import org.scalatest.{FlatSpec, Matchers}

class MyOptionTest extends FlatSpec with Matchers {



  "filter" should "apply to Some class" in {
    MySome(13).filter(_ equals  13) shouldEqual MySome(13)
  }

  it should "not return None class when predicate returns false" in {
    MySome(13).filter(_ equals 12) shouldEqual MyNone
  }

  "flatMap" should "apply mapping function for Some case" in {
    MySome(13).flatMap(v => MySome(v + 2)) shouldEqual MySome(15)
  }

  it should "not apply mapping function for None case" in {
    MySome(13).filter(_ equals 3).flatMap(MySome(_)) shouldEqual MyNone
  }

  "getOrElse" should "return value for Some case" in {
    MySome(13).getOrElse(2) shouldEqual 13
  }

  it should "return default value for None class" in {
    MySome(13).filter(_ equals 12).getOrElse(2) shouldEqual 2
  }

  "map" should "transform values for Some case" in {
    MySome(13).map(_ + 1) shouldEqual MySome(14)
  }

  it should "return map to None when called on None" in {
    MySome(13).map(_ + 1).filter(_ equals 13).map(_ - 1) shouldEqual MyNone
  }
}
