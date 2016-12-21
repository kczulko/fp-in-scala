package com.github.kczulko.chapter11

import com.github.kczulko.chapter6.State

object Monads {
  def optionMonad[A] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  def streamMonad[A] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  class StateMonads[S] {
    type StateS[A] = State[S,A]

    def stateMonad = new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State.unit(a)
      override def flatMap[A, B](ma: StateS[A])(f: (A) => StateS[B]): StateS[B] = ma flatMap f
    }

  }

  def stateMonad[S] = new Monad[({type f[A] = State[S,A]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma flatMap f
  }
}
