package com.github.kczulko.chapter15

sealed trait Process[I,O] {
  def apply(s: Stream[I]): Stream[O] = {
    this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h,t) => h #:: t(s)
    }
  }

  def repeat: Process[I,O] = {
    def loop(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => loop(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => loop(recv(i))
      }
      case Emit(h,t) => Emit(h, loop(t))
    }
    loop(this)
  }
}

case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]
case class Halt[I,O]() extends Process[I,O]

object Process {
  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit[I,O](f(i))
    case None => Halt()
  }

}

