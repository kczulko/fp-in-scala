package com.github.kczulko.chapter11

case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(run(r)).run(r))
}

object Reader {
  def readerMonad[R] = new Monad[({type F[X] = Reader[R,X]})#F] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] = ma flatMap f
  }
}
