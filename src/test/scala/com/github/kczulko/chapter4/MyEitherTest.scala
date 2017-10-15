package com.github.kczulko.chapter4

import com.github.kczulko.chapter4.MyEither._
import org.scalatest.{FlatSpec, Matchers}

class MyEitherTest extends FlatSpec with Matchers {

  behavior of "MyEitherTest"

  it should "return MyRight with list when all elements in arg list are MyRight" in {
    sequence(List(MyRight(1),MyRight(2),MyRight(3))) shouldEqual MyRight(List(1,2,3))
  }

  it should "return MyLeft when any of elements within list is MyLeft" in {
    sequence(List(MyRight(1), MyLeft("s"), MyRight(3), MyLeft("t"))) shouldEqual MyLeft("t")
  }

}
