package com.github.kczulko.chapter13.free

import com.github.kczulko.chapter11.Monad


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
}