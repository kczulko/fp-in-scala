package com.kczulko.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })
  }

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    UnitFuture(f(a(es).get, b(es).get))
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def sortPar[A](parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldLeft(unit(List(): List[A]))((b,a) => if (f(a)) map2(unit(a), b)((a, b) => List(a) ::: b) else b)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
//    es => lazyUnit(ps.map(p => p(es).get())).apply(es)

  /**
    * Prove that map(map(y)(g))(f) == map(y)(f compose g)
    *
    * 1) Let: map(a)(b) == b(a)
    *
    * 2) map(map(y)(g))(f) == map(g(y))(f)
    * 3) map(g(y))(f) == f(g(y))
    *
    * return to 1) and substituting right side of equation with (f compose g)(y):
    * ??? == f(g(y))
    * so ??? == map(y)(f compose g)
    */

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(l: Long, timeUnit: TimeUnit): A = get

    override def cancel(b: Boolean): Boolean = false

    override def isDone: Boolean = true
  }
}
