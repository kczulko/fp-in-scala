package com.github.kczulko.chapter13.tailrec

import com.github.kczulko.chapter11.Monad
import com.github.kczulko.chapter13.{tailrec, _}

import scala.annotation.tailrec

sealed trait TailRec[A] { self =>
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (tailrec.Return(_)))
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = tailrec.FlatMap(this, f)
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](f: () => A) extends TailRec[A]
case class FlatMap[A,B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {
  def empty: TailRec[Unit] = tailrec.Return(())

  override def unit[A](a: => A): TailRec[A] = tailrec.Return(a)

  override def flatMap[A, B](ma: TailRec[A])(f: (A) => TailRec[B]): TailRec[B] = ma flatMap(f)

  def apply[A](a: => A): TailRec[A] = tailrec.Suspend(() => a)

  def ReadLine: TailRec[String] = TailRec { scala.io.StdIn.readLine }
  def PrintLine(msg: String): TailRec[Unit] = tailrec.Suspend(() => tailrec.Return(println(msg)))

  def ref[A](a: A): TailRec[IORef[A]] = TailRec { new IORef(a) }
  sealed class IORef[A](var value: A) {
    def set(a: A): TailRec[A] = TailRec { value = a; a }
    def get: TailRec[A] = TailRec { value }
    def modify(f: A => A): TailRec[A] = get flatMap (a => set(f(a)))
  }

  def forever[F[T] <: { def flatMap[U](f: T => F[U]): F[U] }, A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    a flatMap (_ => t)
  }

  @tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(f) => f()
    case FlatMap(x,f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap(a => g(a) flatMap f))
    }
  }
}