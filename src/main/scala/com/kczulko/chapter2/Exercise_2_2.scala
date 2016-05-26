package com.kczulko.chapter2

import scala.annotation.tailrec

object Exercise_2_2 {
  /*
   * Implement 'isSorted' which checks whether an Array[A]
   * is sorted according to given comparison function 'ordered'
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val isOutOfRange = (nextIndex: Int) => nextIndex >= as.length

    @tailrec
    def loop(index: Int): Boolean = isOutOfRange(index + 1) match {
      case true => true
      case _ => if (!ordered(as(index), as(index + 1))) false
                else loop(index + 1)
    }

    loop(0)
  }
}
