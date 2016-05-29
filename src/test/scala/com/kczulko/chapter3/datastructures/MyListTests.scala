package com.kczulko.chapter3.datastructures

import com.kczulko.chapter3.datastructures.MyList._
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class MyListTests extends FlatSpec with Matchers {
  "tail" should "return list structure without head" in {
    tail(MyList(1,2,3,4)) shouldEqual MyList(2,3,4)
  }

  it should "throw exception when called on Nil" in {
    a [IllegalStateException] should be thrownBy {
      tail(Nil)
    }
  }

  "setHead" should "return new list with changed head" in {
    setHead(MyList(1,2,3,4), 6) shouldEqual MyList(6,2,3,4)
    setHead(Nil, 4) shouldEqual MyList(4)
  }

  "drop" should "remove first two elements from a list" in {
    drop(2, MyList(1,2,3,4)) shouldEqual MyList(3,4)
  }

  it should "remove Nil when requested drops number is greater then list argument size" in {
    drop(20, MyList(1,2,3,4)) shouldEqual Nil
  }

  "dropWhile" should "remove all even values" in {
    dropWhile(MyList(2,4,6,7)){x => x % 2 equals 0} shouldEqual MyList(7)
  }

  "init" should "return a list without it last element" in {
    init(MyList(1,2,3,4)) shouldEqual MyList(1,2,3)
  }

  "length" should "behave list normal lenght function" in {
    MyList.length(MyList(1,2,3,4)) shouldEqual List(1,2,3,4).length
  }

  "foldLeft" should "behave like foldRight" in {
    foldLeft(MyList(1,2,3,4), 0)(_+_) shouldEqual MyList.foldRight(MyList(1,2,3,4), 0)(_+_)
  }

  "lengthAsFoldLeft" should "behave like non-tail-recursive length" in {
    lengthAsFoldLeft(MyList(1,2,3,4,8)) shouldEqual MyList.length(MyList(2,2,2,2,2))
  }

  "reverse" should "work as expected" in {
    reverse(MyList(1,2,3)) shouldEqual MyList(3,2,1)
  }

  "append2" should "return second parameter when called with Nil" in {
    append2(Nil, MyList(3,4)) shouldEqual MyList(3,4)
  }

  it should "return results as append" in {
    append2(MyList(1,2), MyList(3,4)) shouldEqual MyList.append(MyList(1,2), MyList(3,4))
  }

  "append3" should "return second parameter when called with Nil" in {
    append3(Nil, MyList(3,4)) shouldEqual MyList(3,4)
  }

  it should "return results as append" in {
    append3(MyList(1,2), MyList(3,4)) shouldEqual MyList.append(MyList(1,2), MyList(3,4))
  }

  it should "return the same list when appended to Nil list" in {
    append3(MyList(1,2), Nil) shouldEqual MyList(1,2)
  }

  "concat" should "return expected results" in {
    concat(MyList(MyList(1,2), MyList(3,4))) shouldEqual MyList(1,2,3,4)
  }

  "map" should "change each element accordingly with applied function" in {
    map(MyList(1,2))(_+3) shouldEqual MyList(4,5)
  }

  it should "transform list of doubles to list of strings" in {
    map(MyList(1.0,2.0,3.0))(_.toString) shouldEqual MyList("1.0","2.0","3.0")
  }

  "filter" should "remove all matching items from the list" in {
    filter(MyList(1,2,3,4,5,6))(_ % 2 == 0) shouldEqual MyList(1,3,5)
  }

  "flatMap" should "return a list of results" in {
    flatMap(MyList(1,2,3))(i => MyList(i,i)) shouldEqual MyList(1,1,2,2,3,3)
  }

  it should "return a flatten map" in {
    flatMap(MyList(MyList(1,2), MyList(3,4)))(i=>i) shouldEqual MyList(1,2,3,4)
  }

  "filter2" should "remove matching elements using underlying flatMap implementation" in {
    filter2(MyList(1,2,3,4,5))(_ % 2==0) shouldEqual MyList(1,3,5)
  }

  "zipWith" should "add elements of two lists when that fuction is applied as an arg" in {
    zipWith(MyList(1,2,3), MyList(4,5,6))(_ + _) shouldEqual MyList(5,7,9)
  }

  "foldRight" should "return valid product of logical conjunction" in {
    foldRight(MyList(true, false, true, true), true)(_ && _) shouldEqual false
    foldRight(MyList(true, true, true, true), true)(_ && _) shouldEqual true
  }

  "hasSubsequence" should "return expected boolean results" in {
    hasSubsequence(MyList(1,2,3,4,5,6,7), MyList(4,5)) shouldEqual true
    hasSubsequence(MyList(1,2,3,4,5,6,7), MyList(5,4)) shouldEqual false
    hasSubsequence(MyList(1,2,3,4,5,6,7), MyList(5)) shouldEqual true
    hasSubsequence(MyList(1,2,3,4,5,6,7), MyList(1,2,3,4,5)) shouldEqual true
    hasSubsequence(MyList(1,2,3,4,5,6,7), MyList(1,2,3,4,5,6,7)) shouldEqual true
  }
}
