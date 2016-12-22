package com.github.kczulko.chapter12

import com.github.kczulko.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
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

  // (apply,unit) == (map, map2)
  def apply2[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def mapInTermsOfApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)
  def map2InTermsOfApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val fbc = mapInTermsOfApply(fa)(f.curried)
    apply2(fbc)(fb)
  }
}
