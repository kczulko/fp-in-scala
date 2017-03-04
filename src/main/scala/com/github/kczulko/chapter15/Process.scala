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

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f) repeat

  def filter[I](p: I => Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit[I,I](i)
      case _ => Halt()
    } repeat

  def sum: Process[Double,Double] = loop(0.0)((i,s) => (i + s, i + s))

  def take[I](n: Int): Process[I,I] = n match {
    case 0 => Halt()
    case _ => Await[I,I] {
      case Some(i) => Emit[I,I](i, take(n - 1))
      case _ => Halt()
    }
  }

  def drop[I](n: Int): Process[I,I] = n match {
    case 0 => lift(identity)
    case _ => Await[I,I] {
      case Some(_) => drop(n - 1)
      case _ => Halt()
    }
  }

  def takeWhile[I](p: I => Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit(i, takeWhile(p))
      case _ => Halt()
    }

  def dropWhile[I](p: I => Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => dropWhile(p)
      case Some(i) if !p(i) => Emit(i, lift(identity))
      case _ => Halt()
    }

  def count[I]: Process[I,Int] = loop(0)((_,s) => (s + 1, s + 1))

  def mean: Process[Double,Double] =
    loop((0.0, 0))({
      case (cur, (acc, idx)) => ((cur + acc)/(idx + 1), (acc + cur, idx + 1))
    })

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] = {
    Await {
      case Some(i) => f(i,z) match {
        case (o,s2) => Emit(o, loop(s2)(f))
      }
      case _ => Halt()
    }
  }
}
