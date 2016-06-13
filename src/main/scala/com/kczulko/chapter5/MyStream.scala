package com.kczulko.chapter5

import com.kczulko.chapter5.MyStream.cons


trait MyStream[+A] {
  def toList: List[A]
  def take(n: Int): MyStream[A]
  def drop(n: Int): MyStream[A]
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false;
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def forall(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def takeWhile(p: (A) => Boolean): MyStream[A]
    = foldRight(Empty: MyStream[A])((a,b) => if (p(a)) cons(a, b) else Empty)

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): MyStream[B] = foldRight(Empty: MyStream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): MyStream[A] = foldRight(Empty: MyStream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => MyStream[B]): MyStream[B] = s.foldRight(this: MyStream[B])((a, b) => cons(a,b))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = foldRight(Empty: MyStream[B])((a, b) => b append f(a))


}
case object Empty extends MyStream[Nothing] {
  override def toList: List[Nothing] = Nil

  override def take(n: Int): MyStream[Nothing] = this

  override def drop(n: Int): MyStream[Nothing] = this
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
}

object MyStream {
  def cons[A](hd: => A, t: => MyStream[A]): MyStream[A] = {
    lazy val head = hd;
    lazy val tail = t;
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def from(n: Int): MyStream[Int] = cons(n, from(n+1))

  def fib: MyStream[Int] = {
    def loop(a: Int, b: Int): MyStream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }
}
