package com.kczulko.chapter5


trait MyStream[+A] {
  def toList: List[A]
  def take(n: Int): MyStream[A]
  def drop(n: Int): MyStream[A]
  def takeWhile(p: A => Boolean): MyStream[A]
}
case object Empty extends MyStream[Nothing] {
  override def toList: List[Nothing] = Nil

  override def take(n: Int): MyStream[Nothing] = this

  override def drop(n: Int): MyStream[Nothing] = this

  override def takeWhile(p: (Nothing) => Boolean): MyStream[Nothing] = this
}

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] {
  override def toList: List[A] = List(h()) ++ t().toList

  override def take(n: Int): MyStream[A] = n match {
    case 0 => Empty;
    case _ => Cons(h, () => t().take(n - 1))
  }

  override def drop(n: Int): MyStream[A] = n match {
    case 0 => this
    case _ => t().drop(n-1)
  }

  override def takeWhile(p: (A) => Boolean): MyStream[A] = p(h()) match {
    case true => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }
}

object MyStream {
  def cons[A](hd: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val head = hd;
    lazy val tail = t;
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
