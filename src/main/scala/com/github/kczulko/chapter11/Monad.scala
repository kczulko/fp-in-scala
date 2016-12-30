package com.github.kczulko.chapter11

import com.github.kczulko.chapter12.Applicative

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fab: F[(A) => B])(fa: F[A]): F[B] = apply2(fab)(fa)

  override def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case head :: tail => flatMap(f(head))(b => {
      if (!b) filterM(tail)(f) else map(filterM(tail)(f))(head :: _)
    })
  }
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def flatMap2[A,B](ma: F[A])(f: A => F[B]): F[B] = compose(identity[F[A]], f)(ma)
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def flatMap3[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
  def compose2[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}
