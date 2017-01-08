package com.github.kczulko.chapter13

import com.github.kczulko.chapter13.tailrec.TailRec
import org.scalatest.{FlatSpec, Matchers}

class TailRecTest extends FlatSpec with Matchers {
  "trampoline" should "allow to compute fold over 'identity'" in {
    val f: Int => TailRec[Int] = a => tailrec.Suspend(() => a)
    val g = List.fill(10000)(f).foldLeft(f) {
      (a, b) => x => tailrec.Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
    }

    TailRec.run(g(6)) shouldEqual 6
  }
}
