package com.github.kczulko.chapter13.free

import com.github.kczulko.chapter11.Monad

import scala.annotation.tailrec


sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F,B] = flatMap(f andThen(Return(_)))
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
}

case class Return[F[_], A](a: A) extends Free[F,A]
case class Suspend[F[_], A](a: F[A]) extends Free[F,A]
case class FlatMap[F[_], A, B](a: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma flatMap f
    override def unit[A](a: => A): Free[F, A] = Return(a)
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(v) => v
    case Suspend(f) => f()
    case FlatMap(x,f) => x match {
      case Return(v) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y,g) => runTrampoline(y flatMap(g(_) flatMap f))
    }
  }

  def run[F[_],A](a: Free[F,A])(implicit monadF: Monad[F]): F[A] = {
    @tailrec
    def step(v: Free[F,A]): Free[F,A] = a match {
      case FlatMap(FlatMap(x,f),g) => step(x flatMap(f(_) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => v
    }

    step(a) match {
      case Return(a) => monadF.unit(a)
      case Suspend(a) => a
      case FlatMap(x,f) => x match {
        case Suspend(r) => monadF.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible: `step` eliminates this cases.")
      }
    }
  }
}