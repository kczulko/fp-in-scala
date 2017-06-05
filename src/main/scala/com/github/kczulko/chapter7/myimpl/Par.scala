package com.github.kczulko.chapter7.myimpl

import java.util
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

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

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    new Future[C] {

      var invoked: Option[util.List[Future[Any]]] = None

      val aCallable = new Callable[A] {
        override def call(): A = a(es).get()
      }
      val bCallable = new Callable[B] {
        override def call(): B = b(es).get()
      }

      private def await[V](future: => Future[V]): V = {
        while (!future.isDone && !future.isCancelled) {}
        future.get()
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
        val first = await(all.get(0).asInstanceOf[Future[A]])
        val second = await(all.get(1).asInstanceOf[Future[B]])

        f(first, second)
      }
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](p: Par[A])(f: A => B): Par[B] = {
    map2(p, unit(())){ case (a,_) => f(a) }
  }
}
