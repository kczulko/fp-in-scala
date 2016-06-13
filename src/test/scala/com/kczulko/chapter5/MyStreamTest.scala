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

  "takeWhile" should "return elements from the Stream that are passing given predicate" in {
    (MyStream(1,2,3,4) takeWhile { _ < 3 }).toList shouldEqual MyStream(1,2).toList
    (MyStream(1,2) drop 4 takeWhile {_ < 2 }).toList shouldEqual MyStream().toList
    (MyStream() takeWhile {_ => true}).toList shouldEqual MyStream().toList
  }

  "forall" should "return boolean indicator valid for all stream elements" in {
    MyStream(1,2,3,4) forall { _ < 5 } shouldEqual true
    MyStream(1,2,3,4) forall { _ > 0 } shouldEqual true
    MyStream(1,2,3,4) forall { _ < 4 } shouldEqual false
  }

  "headOption" should "return first element of the Stream" in {
    MyStream(1,2,3,4).headOption shouldEqual Some(1)
    MyStream().headOption shouldEqual None
  }

  "map" should "transform each element in the stream with applied method" in {
    { MyStream(1,2,3,4) map {_ + 1} toList } shouldEqual List(2,3,4,5)
  }

  "filter" should "remove elements from the stream that don't satisfy predicate" in {
    { MyStream(1,2,3,4,5) filter {_ % 2 equals 0} toList } shouldEqual List(2,4)
  }

  "append" should "add given stream on the beginning of the caller stream" in {
    { MyStream(3,4,5,6) append MyStream(1,2) toList } shouldEqual List(1,2,3,4,5,6)
  }

  "flatMap" should "return a flatten stream" in {
    { MyStream(MyStream(1,2), MyStream(3,4)) flatMap(x => x) toList } shouldEqual List(1,2,3,4)
  }
}