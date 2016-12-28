package com.github.kczulko.chapter12

import com.github.kczulko.chapter10.Foldable
import com.github.kczulko.chapter11.Functor
import com.github.kczulko.chapter12.Applicatives.optionApp

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] = {
    traverse(fa)(a => optionApp.unit(f(a))) match {
      case Some(fb) => fb
      case None => throw new IllegalStateException("inconceivable!")
    }
  }
}
