package com.kczulko.chapter5

import org.scalatest.{FlatSpec, Matchers}

class MyStreamTest extends FlatSpec with Matchers {

  "toList" should "return list represenation of the stream" in {
    MyStream(1,2,3,4).toList shouldEqual List(1,2,3,4)
  }

  "take" should "return first n elements of the stream" in {
    (MyStream.empty take 2).toList shouldEqual MyStream.empty.toList
    (MyStream(1,2,3,4) take 2).toList shouldEqual MyStream(1,2).toList
    (MyStream(1,2,3,4) take 4).toList shouldEqual MyStream(1,2,3,4).toList
    (MyStream(1,2,3,4) take 5).toList shouldEqual MyStream(1,2,3,4).toList
  }

  "drop" should "remove first n elements from the stream" in {
    (MyStream(1,2,3,4) drop 2).toList shouldEqual MyStream(3,4).toList
    (MyStream(1,2,3,4) drop 4).toList shouldEqual MyStream().toList
    (MyStream(1,2,3,4) drop 5).toList shouldEqual MyStream().toList
  }

}
