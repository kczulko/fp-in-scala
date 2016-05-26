package com.kczulko.chapter3.datastructures

import com.kczulko.chapter3.datastructures.MyList.{drop, dropWhile, setHead, tail}
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
    MyList.init(MyList(1,2,3,4)) shouldEqual MyList(1,2,3)
  }
}
