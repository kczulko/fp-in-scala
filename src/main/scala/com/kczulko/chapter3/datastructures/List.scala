package com.kczulko.chapter3.datastructures

import scala.annotation.tailrec

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def init[A](myList: MyList[A]): MyList[A] = myList match {
    case Nil => Nil
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def tail[A](myList: MyList[A]): MyList[A] = myList match {
    case Nil => throw new IllegalStateException("tail on Nil")
    case Cons(h, t) => t
  }

  @tailrec
  def drop[A](n: Int, myList: MyList[A]) : MyList[A] = n match {
    case 0 => myList
    case _ => myList match {
      case Nil => Nil
      case Cons(h, t) => drop(n - 1, t)
    }
  }

  @tailrec
  def dropWhile[A](myList: MyList[A])(f: A => Boolean): MyList[A] = myList match {
    case Cons(h, t) if (f(h)) => dropWhile(t)(f)
    case _ => myList
  }

  def setHead[A](myList: MyList[A], newHead: A): MyList[A] = myList match {
    case Nil => MyList(newHead)
    case Cons(h, t) => Cons(newHead, t)
  }

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): MyList[A] = if (as isEmpty) Nil
                                    else Cons(as.head, apply(as.tail: _*))


}