package com.github.kczulko.chapter7.blocking

import java.util.concurrent.{CancellationException, TimeUnit}

import com.github.kczulko.chapter7.ExecutionContext.executionContext
import org.scalatest.{FlatSpec, Matchers}

class ParTestBlocking extends FlatSpec with Matchers {

  private def delayValue[A](a: A, delayMs: Long): Par.Par[A] =
    Par.lazyUnit({Thread.sleep(delayMs); a})

  "map2" should "respect termination unit assigned to the tasks" in executionContext(4) {
    es =>
      val futureResult = Par.map2(delayValue(4, 300), delayValue(5, 300))(_ + _)(es)
      futureResult.get(330, TimeUnit.MILLISECONDS) shouldEqual 9
  }

  it should s"throw ${classOf[CancellationException]} when requested timeout is too small" in executionContext(2) {
    es =>
      an [CancellationException] should be thrownBy {
        val futureResult = Par.map2(delayValue(4, 300), delayValue(5, 310))(_ + _)(es)
        futureResult.get(290, TimeUnit.MILLISECONDS)
      }
  }

  "parFilter" should "hold elements that passes given predicate" in executionContext(2) {
    es =>
      val futureResult = Par.parFilter(List[Int](1, 2, 3, 4, 5, 6, 7, 8))(_ > 5)(es)
      futureResult.get() should contain theSameElementsInOrderAs List(6,7,8)
  }

  "map" should "satisfy simple mapping law when called with unit() function" in executionContext(2) {
    es =>
      Par.map(Par.unit(3))(_ + 1)(es).get() shouldEqual 4
  }

  "choice" should "return expected value depending on the result of passed condition" in executionContext(5) {
    es =>
      Par.run(es) {
        Par.choice(Par.lazyUnit(true))(Par.lazyUnit(8), Par.lazyUnit(7))
      }.get() shouldEqual 8

      Par.run(es) {
        Par.choice(Par.lazyUnit(false))(Par.lazyUnit(8), Par.lazyUnit(7))
      }.get() shouldEqual 7
  }

  "choiceN" should "return value under requested index from passed list" in executionContext(5) {
    es => {
      val parDigits = (0 to 10).map(Par.lazyUnit(_)).toList
      parDigits.zipWithIndex.foreach {
        case (par, index) =>
          Par.run(es)(Par.choiceN(par)(parDigits)).get() shouldEqual index
      }
    }
  }
}