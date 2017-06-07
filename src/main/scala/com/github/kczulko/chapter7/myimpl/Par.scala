package com.github.kczulko.chapter7.myimpl

import java.util
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.collection.immutable

object Par {
  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = true
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def fork[A](a: => Par[A] ): Par[A] = es => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })
  }
  // caller of run can choose when to timeout etc.
  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] =
    map2(map2(a,b)(f.curried(_)(_)), c)(_(_))

  def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] =
    map2(map2(map2(a,b)(f.curried(_)(_)), c)(_(_)), d)(_(_))

  def map5[A,B,C,D,E,F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] =
    map2(map4(a,b,c,d)(f.curried(_)(_)(_)(_)), e)(_(_))

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es =>
    new Future[C] {

      val aCallable = new Callable[A] {
        override def call(): A = a(es).get()
      }
      val bCallable = new Callable[B] {
        override def call(): B = b(es).get()
      }

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      override def isCancelled: Boolean = false
      override def isDone: Boolean = false
      override def get(): C = {
        val left = a(es).get()
        val right = b(es).get()
        f(left, right)
      }
      override def get(timeout: Long, unit: TimeUnit): C = {
        val callables = new util.ArrayList[Callable[Any]]()
        callables.add(aCallable.asInstanceOf[Callable[Any]])
        callables.add(bCallable.asInstanceOf[Callable[Any]])
        val all = es.invokeAll(callables, timeout, unit)

        // wait for the result
        val first = all.get(0).asInstanceOf[Future[A]].get()
        val second = all.get(1).asInstanceOf[Future[B]].get()

        f(first, second)
      }
    }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](p: Par[A])(f: A => B): Par[B] = {
    map2(p, unit(())){ case (a,_) => f(a) }
  }

  def parMap[A,B](list: List[A])(f: A => B): Par[List[B]] = fork {
    val listOfPars: List[Par[B]] = list.map(asyncF(f))
    sequence(listOfPars)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List[A]()))((parA, parList) => map2(parA, parList)(_ :: _))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val filteredList = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(filteredList))(_.flatten)
  }

  def delay[A](par: => Par[A]): Par[A] = es => par(es)
}
