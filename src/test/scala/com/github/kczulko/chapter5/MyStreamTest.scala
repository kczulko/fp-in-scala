package com.github.kczulko.chapter5

import MyStream.constant
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

  "constant" should "return stream of constant numbers" in {
    { constant(6) take 5 toList } shouldEqual List(6,6,6,6,6)
  }

  "from" should "create an infinite stream with given start number" in {
    { MyStream.from(4) take 3 toList } shouldEqual List(4,5,6)
  }

  "fib" should "return stream of fibbonacci numbers" in {
    { MyStream.fib take 6 toList } shouldEqual List(0,1,1,2,3,5)
  }

  "zipWith" should "concat two streams accordingly to given function" in {
    MyStream(1,2,3,4,5).zipWith(MyStream(5,4,3,2,1))((x,y) => s"$x$y" toInt).toList shouldEqual List(15,24,33,42,51)
  }

  "zipAll" should "return stream of Options" in {
    { MyStream(1,2,3) zipAll(MyStream(4,5)) toList } shouldEqual List(
      (Some(1), Some(4)),
      (Some(2), Some(5)),
      (Some(3), None)
    )
  }

  "startsWith" should "return true when Stream starts with another one" in {
    MyStream(1,2,3,4) startsWith MyStream(1,2) should be {true}
  }

  it should "return false when Stream does not start with another one" in {
    MyStream(1,2,3,4) startsWith MyStream(2, 3) should not be {true}
  }

  it should "return false when Empty stream passed as an argument" in {
    MyStream(1,2,3) startsWith Empty shouldEqual false
  }

  it should "return false when called on empty Stream" in {
    MyStream() startsWith MyStream(1,2) shouldEqual false
  }

  "tails" should "return the stream of suffixes when called on a stream" in {
    { MyStream(1,2,3) tails }.map(_.toList).toList shouldEqual List(List(1,2,3), List(2,3), List(3), List())
  }

  "scanRight" should "return sums of each product that comes from 'tails'" in {
    { MyStream(1,2,3).scanRight(0)(_+_) toList } shouldEqual List(6,5,3,0)
  }

  "tails2" should "" in {
    { MyStream(1,2,3) tails2 }.map(_.toList).toList shouldEqual List(List(1,2,3), List(2,3), List(3), List())
  }
}