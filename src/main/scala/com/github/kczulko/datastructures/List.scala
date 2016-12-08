package com.github.kczulko.chapter3.datastructures

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

  def product(ds: MyList[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def sum2(ints: MyList[Int]): Int = foldRight(ints, 0)(_ + _)

  def foldRight[A,B](list: MyList[A], base: B)(f: (A, B) => B): B = list match {
    case Nil => base
    case Cons(head, tail) => f(head, foldRight(tail, base)(f))
  }

  def foldRight2[A,B](list: MyList[A], base: B)(f: (A, B) => B): B =
    foldLeft(list, base)((x,y) => f(y,x))

  @tailrec
  def foldLeft[A,B](list: MyList[A], base: B)(f: (B,A) => B): B = list match {
    case Nil => base
    case Cons(head, tail) => foldLeft(tail, f(base, head))(f)
  }

  @tailrec
  def foldLeft2[A,B](list: MyList[A], base: B)(f: (B,A) => B): B = {
    println("Called with params:")
    println("list : " + list)
    println("base : " + base)
    println("f : " + f)

    list match {
      case Nil => base
      case Cons(head, tail) => foldLeft2(tail, f(base, head))(f)
    }
  }

  def concat[A](listOfLists: MyList[MyList[A]]): MyList[A] = {
    foldLeft(listOfLists, Nil:MyList[A])((x,y) => append(x, y))
  }

  def append2[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(reverse(a1), a2)((x,y) => Cons(y, x))

  def append3[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRight(a1, a2)((x,y) => Cons(x, y))

  def sumAsFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productAsFoldLeft(ints: MyList[Double]): Double = foldLeft(ints, 1.0)(_ * _)

  def lengthAsFoldLeft[A](list: MyList[A]): Int = foldLeft(list, 0)((x,y)=>x + 1)

  def length[A](list: MyList[A]): Int = foldRight(list, 0)((x,y)=>1+y)

  def reverse[A](list: MyList[A]) = foldLeft(list, Nil: MyList[A])((x,y) => Cons(y,x))

  def apply[A](as: A*): MyList[A] = if (as isEmpty) Nil
                                    else Cons(as.head, apply(as.tail: _*))

  def map[A,B](list: MyList[A])(f: A => B) : MyList[B] =
    foldRight(list, Nil:MyList[B])((x,y) => Cons(f(x),y))

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, Nil:MyList[A])((x,y) => if(!f(x)) Cons(x, y) else y)

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldRight(as, Nil:MyList[B])((x,y) => append(f(x),y))

  def filter2[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(i => if (f(i)) Nil else MyList(i))

  def zipWith[A,B](first:MyList[A], second: MyList[A])(f: (A,A) => B): MyList[B] = (first,second) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(aHead,aTail), Cons(bHead,bTail))
      => Cons(f(aHead, bHead), zipWith(aTail,bTail)(f))
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    def loop(one: MyList[A], two: MyList[A], results: MyList[Boolean]): MyList[Boolean] = (one, two) match {
      case (_, Nil) => results
      case (Nil, _) => results
      case (Cons(hOne, tOne), Cons(hTwo, tTwo)) if (hOne == hTwo)
        => loop(tOne, tTwo, append(MyList(true), results))
      case (Cons(hOne, tOne), _) => loop(tOne, sub, Nil)
    }

    val results = loop(sup, sub, Nil)

    length(results) == length(sub) && foldLeft(results, true)(_ && _)
  }
}
