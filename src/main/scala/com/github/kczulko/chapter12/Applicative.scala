package com.github.kczulko.chapter12

import com.github.kczulko.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, fa)((a, _) =>f(a))
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,flb) => map2(f(a), flb)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = map(fa)(List.fill(n)(_))
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((_,_))
}
