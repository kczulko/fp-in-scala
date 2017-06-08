package com.github.kczulko.chapter7.nonblocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import akka.actor.{Actor, ActorSystem, Props}

object Par {
  sealed trait Future[+A] {
    private[nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)

    p(es) { v =>
      ref.set(v)
      latch.countDown()
    }
    // await for the result
    latch.await()

    // return the value
    ref.get()
  }

  def map[A,B](p: Par[A])(f: A => B): Par[B] = {
    map2(p, unit(())){ case (a,_) => f(a) }
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    override private[nonblocking] def apply(k: (A) => Unit) = k(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override private[nonblocking] def apply(k: (A) => Unit) = {
      eval(es)(a(es)(k))
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = {
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })
  }

  private class MapActor[A, B](f: (A,B) => Unit, es: ExecutorService) extends Actor {
    var aRes: Option[A] = None
    var bRes: Option[B] = None

    override def receive: Receive = {
      case Left(l) if l.isInstanceOf[A] => bRes match {
        case Some(bb) =>
          eval(es)(f(l.asInstanceOf[A], bb))
          context stop self
        case None => aRes = Some(l.asInstanceOf[A])
      }
      case Right(r) if r.isInstanceOf[B] => aRes match {
        case Some(aa) =>
          eval(es)(f(aa, r.asInstanceOf[B]))
          context stop self
        case None => bRes = Some(r.asInstanceOf[B])
      }
    }
  }

  private case class Left[A](a: A)
  private case class Right[B](b: B)
  val system = ActorSystem()

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => new Future[C] {
    override private[nonblocking] def apply(k: (C) => Unit) = {
      // using passed executor service is causing StackOverflowException ...
      //val system = ActorSystem("map2", defaultExecutionContext = Some(ExecutionContext.fromExecutorService(es)))
//      val system = ActorSystem()
      val mapActor = system.actorOf(Props.apply(classOf[MapActor[A,B]], (a: A, b: B) => k(f(a,b)), es))
      a(es)(aa => mapActor ! Left(aa))
      b(es)(bb => mapActor ! Right(bb))
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A,B](list: List[A])(f: A => B): Par[List[B]] = fork {
    val listOfPars: List[Par[B]] = list.map(asyncF(f))
    sequence(listOfPars)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List[A]()))((parA, parList) => map2(parA, parList)(_ :: _))
}
