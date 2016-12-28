package com.github.kczulko.chapter12

import com.github.kczulko.chapter10.Foldable
import com.github.kczulko.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
}
