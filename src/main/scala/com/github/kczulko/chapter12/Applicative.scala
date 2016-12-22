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
    apply2(unit[A => B](f))(fa)
  def map2InTermsOfApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val fbc = mapInTermsOfApply(fa)(f.curried)
    apply2(fbc)(fb)
  }

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val fbcd = mapInTermsOfApply(fa)(f.curried)
    val fcd = map2InTermsOfApply(fbcd, fb)(_(_))
    apply2(fcd)(fc)
  }
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = {
    val fbcde = mapInTermsOfApply(fa)(f.curried)
    val fcde = map2InTermsOfApply(fbcde, fb)(_(_))
    val fde = map2InTermsOfApply(fcde, fc)(_(_))
    apply2(fde)(fd)
  }
}
