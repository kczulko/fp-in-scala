package com.github.kczulko.chapter11

import com.github.kczulko.chapter6.State
import com.github.kczulko.chapter7.Nonblocking.Par

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

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
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

  def eitherMonad[E]: Monad[({type F[X] = Either[E,X]})#F] = new Monad[({type F[X] = Either[E, X]})#F] {
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
    override def unit[A](a: => A) = Right(a)
  }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](ma: () => A)(f: (A) => () => B): () => B = f(ma())
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

}
