package com.github.kczulko.chapter13.async

import com.github.kczulko.chapter11.Monad
import com.github.kczulko.chapter7.Nonblocking.Par

import scala.annotation.tailrec

sealed trait Async[A] {
  def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
  def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends Async[A]
case class Suspend[A](a: Par[A]) extends Async[A]
case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

object Async extends Monad[Par] {

  def unit[A](a: => A): Par[A] = Par.unit(a)
  def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)

  @tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x,f),g) => step(x flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(a) => a
    case FlatMap(x,f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible: `step` eliminates this cases.")
    }
  }
}