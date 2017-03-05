package com.github.kczulko.chapter15

import com.github.kczulko.chapter15.Process._
import org.scalatest.{FlatSpec, Matchers}

class ProcessTest extends FlatSpec with Matchers {
  "liftOne" should "modify first element of the given stream" in {
    liftOne((i: Int) => i + 4)(Stream(3,4,5,6)).toList shouldEqual List(7)
  }

  "repeat" should "apply given function to all stream elements" in {
    liftOne[Int,Int](_ + 4).repeat(Stream(3,4,5,6)).toList should
      contain theSameElementsAs (7 to 10)
  }

  "lift" should "modify all elements of the given stream" in {
    lift[Int,Int](_ + 4)(Stream(1,2,3,4)).toList shouldEqual List(5,6,7,8)
  }

  "filter" should "remove elements that fails given predicate" in {
    filter[Int](_ % 2 == 0)(Stream(1,2,3,4,5)).toList shouldEqual List(2,4)
  }

  "sum" should "incrementally add elements within a given stream" in {
    sum(Stream(1,2,3,4)).toList shouldEqual List(1,3,6,10)
  }

  "take" should "return amount of elements given as an argument" in {
    take(4)(Stream(2,3,4,5,6,7,8)).toList shouldEqual List(2,3,4,5)
  }

  "drop" should "skip amount of elements given as an argument" in {
    drop(3)(Stream(1,2,3,4,5,6)).toList shouldEqual List(4,5,6)
  }

  "takeWhile" should "return elements as long as the predicate is satisfied" in {
    takeWhile[Int](_ < 5)(Stream(1,2,3,4,5,6,7)).toList should
      contain theSameElementsAs (1 to 4)
  }

  "dropWhile" should "remove elements from the beginning that does not fit given predicate" in {
    dropWhile[Int](_ < 4)(Stream(1,2,3,4,5,6,7,1)).toList shouldEqual List(4,5,6,7,1)
  }

  "count" should "just count :) elements in the input stream" in {
    count(Stream(8,4,7,2,6)).toList shouldEqual (1 to 5 toList)
    count(Stream()).toList shouldBe empty
  }

  "mean" should "return stream of mean values accordingly to the order of the stream" in {
    mean(Stream(1,2,3,4,5)).toList shouldEqual List(1, 1.5, 2, 2.5, 3)
    mean(Stream(0)).toList shouldEqual List(0)
    mean(Stream()).toList shouldBe empty
  }

  "|> operator" should "allow to write pipe of processes" in {
    val pipe = lift[Int,Int](_ + 2) |> filter[Int](_ % 2 == 0)
    pipe(Stream(1,2,3,4)).toList shouldEqual List(4,6)
  }

  "zipWithIndex" should "emit a running count of values emitted along with each value" in {
    lift[Int,Int](identity).zipWithIndex(Stream(5,4,3,2,1)).toList shouldEqual
      List((5,0),(4,1),(3,2),(2,3),(1,4))
  }
}
