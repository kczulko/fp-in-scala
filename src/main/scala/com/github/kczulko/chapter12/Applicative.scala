package com.github.kczulko.chapter12

import com.github.kczulko.chapter10.Monoid
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
  def assoc[A,B,C](p: (A,(B,C))): ((A,B),C) =
    p match { case (a,(b,c)) => ((a,b),c) }

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

  def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
  new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (
      Applicative.this.map2(fa._1, fb._1)(f),
      g.map2(fa._2, fb._2)(f)
    )
    override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
      apply2(fab)(fa)
    override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), g.unit(a))
    override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = (
      Applicative.this.map(fa._1)(f),
      g.map(fa._2)(f)
    )
  }

  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        Applicative.this.map2(fa, fb)(g.map2(_,_)(f))
      override def apply[A, B](fab: F[G[(A) => B]])(fa: F[G[A]]): F[G[B]] =
        apply2(fab)(fa)
      override def unit[A](a: => A): F[G[A]] = Applicative.this.unit(g.unit(a))
      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] =
        Applicative.this.map(fa)(g.map(_)(f))
    }

  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map[K,V]())) { (pair, b) =>
      map2(pair._2, b)((v, l) => l + (pair._1 -> v))
    }
}

object Applicative {
  type Const[M,B] = M

  implicit def monoidApplicative[M](m: Monoid[M]): Applicative[({type f[x] = Const[M,x]})#f] =
    new Applicative[({type f[x] = _root_.com.github.kczulko.chapter12.Applicative.Const[M, x]})#f] {
      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = m.op(fa,fb)
      override def apply[A, B](fab: Const[M, (A) => B])(fa: Const[M, A]): Const[M, B] = apply2(fab)(fa)
      override def unit[A](a: => A): Const[M, A] = m.zero
    }
}
