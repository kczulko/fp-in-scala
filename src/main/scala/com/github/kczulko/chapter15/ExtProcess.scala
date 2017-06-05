package com.github.kczulko.chapter15

import com.github.kczulko.chapter13.free.Free.IO
import com.github.kczulko.chapter15.ExtProcess._

trait ExtProcess[F[_],O] {

  def onHalt(f: Throwable => ExtProcess[F,O]): ExtProcess[F,O] = this match {
    case ExtHalt(e) => Try(f(e))
    case ExtEmit(h,t) => ExtEmit(h, t.onHalt(f))
    case ExtAwait(req,recv) => await(req)(eta => recv(eta).onHalt(f))
  }

  def ++(p: => ExtProcess[F,O]): ExtProcess[F,O] = this.onHalt {
    case End => p
    case err => ExtHalt(err)
  }

  def Try[F[_],O](p: => ExtProcess[F,O]): ExtProcess[F,O] =
    try p
    catch {
      case e: Throwable => ExtHalt(e)
    }

  def flatMap[O2](f: O => ExtProcess[F,O2]): ExtProcess[F,O2] = this match {
    case ExtHalt(e) => ExtHalt(e)
    case ExtEmit(h,t) => f(h) ++ t.flatMap(f)
    case ExtAwait(req,recv) => ExtAwait(req, recv andThen(_ flatMap f))
  }

  def onComplete(p: => ExtProcess[F,O]): ExtProcess[F,O] =
    this.onHalt({
      case End => p.asFinalizer
      case err => p.asFinalizer ++ ExtHalt(err)
    })

  def asFinalizer: ExtProcess[F,O] = this match {
    case h @ ExtHalt(e) => h
    case ExtEmit(h,t) => ExtEmit(h, t.asFinalizer)
    case ExtAwait(req,recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }
}

object ExtProcess {
  case class ExtAwait[F[_],A,O](req: F[A], recv: Either[Throwable, A] => ExtProcess[F,O])
    extends ExtProcess[F,O]

  case class ExtEmit[F[_],O](head: O, tail: ExtProcess[F,O]) extends ExtProcess[F,O]

  case class ExtHalt[F[_],O](err: Throwable) extends ExtProcess[F,O]

  case object End extends Exception
  case object Kill extends Exception

  def resource[R,O](acquire: IO[R],
                    use: R => ExtProcess[IO,O],
                    release: R => ExtProcess[IO,O]): ExtProcess[IO,O] =
    await[IO,R,O](acquire) {
      case Right(r) => use(r).onComplete(release(r))
      case Left(err) => ExtHalt(err) /// ???
    }

  def await[F[_],A,O](req: F[A])(recv: Either[Throwable,A] => ExtProcess[F,O]): ExtProcess[F,O] =
    ExtAwait(req, recv)

  def eval[F[_],A](a: F[A]): ExtProcess[F,A] =
    await(a) {
      case Right(r) => ExtEmit(r, ExtHalt(End))
      case Left(err) => ExtHalt(err)
    }

  def eval_[F[_],A,B](a: F[A]): Process[F[_],B] = ???
}
