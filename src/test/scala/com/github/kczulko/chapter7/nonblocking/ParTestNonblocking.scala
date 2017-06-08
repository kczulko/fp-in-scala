package com.github.kczulko.chapter7.nonblocking

import akka.actor.ActorSystem
import com.github.kczulko.chapter7.ExecutionContext.executionContext
import org.scalatest.{FlatSpec, Matchers}

class ParTestNonblocking extends FlatSpec with Matchers {

  "run" should "execute simple task" in executionContext(2) { es =>
    Par.run(es)(Par.fork(Par.unit({Thread.sleep(500); 5}))) shouldEqual 5
  }

  ignore should "swallow exceptions" in executionContext(3) {
    es =>
      lazy val thr = throw new IllegalArgumentException
      Par.run(es)(Par.lazyUnit(thr))
  }

  "map2" should "properly combine elements for map2 method when implemented with Akka Actors" in executionContext(2) { es =>
    val first = Par.fork(Par.unit({Thread.sleep(500); 5}))
    val second = Par.fork(Par.unit({Thread.sleep(500); 5}))

    Par.run(es)(Par.map2(first, second)(_ + _)) shouldEqual 10
  }

  it should "spawn many actors for some simple mapping" in executionContext(2) { es => ()
    val list = List.range(1, 1000)
    val negated = Par.parMap(list)(_ * (-1))
    val zipped = Par.map2(Par.unit(list), negated)(_ zip _)

    Par.run(es) {
      Par.run(es) {
        Par.map(zipped)(z => Par.parMap(z){ case (l,r) => l+r })
      }
    } should contain only 0
  }

}
