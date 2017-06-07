package com.github.kczulko.chapter7.myimpl

import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date
import java.util.concurrent.{CancellationException, ExecutorService, Executors, TimeUnit}

import org.scalatest.{FlatSpec, Matchers}

class ParTest extends FlatSpec with Matchers {

  def executionContext(test: ExecutorService => Any): Unit = {
    val pool: ExecutorService = Executors.newFixedThreadPool(8)
    test(pool)
    pool.shutdownNow()
  }

  private def delayValue[A](a: A, delayMs: Long): Par.Par[A] =
    Par.lazyUnit({Thread.sleep(delayMs); a})

  "map2" should "respect termination unit assigned to the tasks" in executionContext {
    es =>
      val futureResult = Par.map2(delayValue(4, 300), delayValue(5, 300))(_ + _)(es)
      futureResult.get(330, TimeUnit.MILLISECONDS) shouldEqual 9
  }

  it should s"throw ${classOf[CancellationException]} when requested timeout is too small" in executionContext {
    es =>
      an [CancellationException] should be thrownBy {
        val futureResult = Par.map2(delayValue(4, 300), delayValue(5, 310))(_ + _)(es)
        futureResult.get(290, TimeUnit.MILLISECONDS)
      }
  }

  "parFilter" should "hold elements that passes given predicate" in executionContext {
    es =>
      val futureResult = Par.parFilter(List[Int](1, 2, 3, 4, 5, 6, 7, 8))(_ > 5)(es)
      futureResult.get() should contain theSameElementsInOrderAs List(6,7,8)
  }

  "map" should "satisfy simple mapping law when called with unit() function" in executionContext {
    es =>
      Par.map(Par.unit(3))(_ + 1)(es).get() shouldEqual 4
  }
}