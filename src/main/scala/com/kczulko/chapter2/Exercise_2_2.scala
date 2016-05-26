package com.kczulko.chapter2

import scala.annotation.tailrec

object Exercise_2_2 {
  /*
   * Implement 'isSorted' which checks whether an Array[A]
   * is sorted according to given comparison function 'ordered'
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val isOutOfRange = (nextIndex: Int) => nextIndex >= as.length

    if (List(0, 1) contains as.length) {
      return true
    }

    @tailrec
    def loop(elem: Int, nextElem: Int): Boolean =
    ordered(as(elem), as(nextElem)) match {
      case false => false
      case _ => if (isOutOfRange(nextElem + 1)) true else loop(nextElem, nextElem + 1)
    }

    loop(0, 1)
  }
}
